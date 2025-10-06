unit avltree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

/// Genera un reporte (DOT + PNG) del AVL construido con los drafts (draft.json).
function GenerateAVLReportFromDrafts(const outputFolder: string): Boolean;

implementation

uses
  fpjson, jsonparser, Process, Unix,
  variables, jsonTools,
  interfaceTools;

type
  PAVLNode = ^TAVLNode;
  TAVLNode = record
    id: Integer;
    txt: string;
    left, right: PAVLNode;
    height: Integer;
  end;

var
  rootAVL: PAVLNode = nil;

// ---------------- AVL helpers ----------------
function NodeHeight(n: PAVLNode): Integer;
begin
  if n = nil then Result := 0 else Result := n^.height;
end;

procedure UpdateHeight(n: PAVLNode);
var
  hl, hr: Integer;
begin
  if n = nil then Exit;
  hl := NodeHeight(n^.left);
  hr := NodeHeight(n^.right);
  if hl > hr then n^.height := hl + 1 else n^.height := hr + 1;
end;

function BalanceFactor(n: PAVLNode): Integer;
begin
  if n = nil then Result := 0 else Result := NodeHeight(n^.left) - NodeHeight(n^.right);
end;

function RotateRight(y: PAVLNode): PAVLNode;
var x: PAVLNode;
begin
  x := y^.left;
  y^.left := x^.right;
  x^.right := y;
  UpdateHeight(y);
  UpdateHeight(x);
  Result := x;
end;

function RotateLeft(x: PAVLNode): PAVLNode;
var y: PAVLNode;
begin
  y := x^.right;
  x^.right := y^.left;
  y^.left := x;
  UpdateHeight(x);
  UpdateHeight(y);
  Result := y;
end;

function CreateNode(aid: Integer; const alabel: string): PAVLNode;
var n: PAVLNode;
begin
  New(n);
  n^.id := aid;
  n^.txt := alabel;
  n^.left := nil;
  n^.right := nil;
  n^.height := 1;
  Result := n;
end;

function AVLInsert(node: PAVLNode; aid: Integer; const alabel: string): PAVLNode;
var balance: Integer;
begin
  if node = nil then
  begin
    Result := CreateNode(aid, alabel);
    Exit;
  end;

  if aid < node^.id then
    node^.left := AVLInsert(node^.left, aid, alabel)
  else if aid > node^.id then
    node^.right := AVLInsert(node^.right, aid, alabel)
  else
  begin
    node^.txt := alabel;
    Result := node;
    Exit;
  end;

  UpdateHeight(node);
  balance := BalanceFactor(node);

  if (balance > 1) and (aid < node^.left^.id) then
  begin
    Result := RotateRight(node);
    Exit;
  end;

  if (balance < -1) and (aid > node^.right^.id) then
  begin
    Result := RotateLeft(node);
    Exit;
  end;

  if (balance > 1) and (aid > node^.left^.id) then
  begin
    node^.left := RotateLeft(node^.left);
    Result := RotateRight(node);
    Exit;
  end;

  if (balance < -1) and (aid < node^.right^.id) then
  begin
    node^.right := RotateRight(node^.right);
    Result := RotateLeft(node);
    Exit;
  end;

  Result := node;
end;

procedure FreeAVL(node: PAVLNode);
begin
  if node = nil then Exit;
  FreeAVL(node^.left);
  FreeAVL(node^.right);
  Dispose(node);
end;

// ---------------- DOT generation ----------------
procedure EscapeAndWriteLabel(const s: string; out outS: string);
begin
  outS := StringReplace(s, '\', '\\', [rfReplaceAll]);
  outS := StringReplace(outS, '"', '\"', [rfReplaceAll]);
  outS := StringReplace(outS, sLineBreak, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #13#10, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #13, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #10, '\n', [rfReplaceAll]);
end;

procedure WriteDotNode(var f: Text; node: PAVLNode);
var lab: string;
begin
  if node = nil then Exit;
  EscapeAndWriteLabel(node^.txt, lab);
  // 👇 Aquí agregamos correctamente el label multilínea (con \n reales)
  Writeln(f, Format('  "n%d" [label="%s", shape=box, style=filled, fillcolor=lightgoldenrod, fontname="Helvetica", fontsize=10];', [node^.id, lab]));
  if node^.left <> nil then
    Writeln(f, Format('  "n%d" -> "n%d";', [node^.id, node^.left^.id]));
  if node^.right <> nil then
    Writeln(f, Format('  "n%d" -> "n%d";', [node^.id, node^.right^.id]));
  WriteDotNode(f, node^.left);
  WriteDotNode(f, node^.right);
end;

function GenerateDOTFile(const dotPath: string): Boolean;
var f: Text;
begin
  Result := False;
  try
    AssignFile(f, dotPath);
    Rewrite(f);
    try
      Writeln(f, 'digraph AVL {');
      Writeln(f, '  rankdir=TB;');
      Writeln(f, '  node [shape=box, style=filled, fillcolor=lightyellow, fontname="Helvetica"];');
      if rootAVL = nil then
        Writeln(f, '  empty [label="(vacio)"];')
      else
        WriteDotNode(f, rootAVL);
      Writeln(f, '}');
      Result := True;
    finally
      CloseFile(f);
    end;
  except
    Result := False;
  end;
end;

// ---------------- Run Graphviz ----------------
function RunGraphviz(const dotPath, pngPath: string): Boolean;
var
  cmd: string;
  exitCode: LongInt;
begin
  Result := False;
  cmd := Format('dot -Tpng "%s" -o "%s"', [dotPath, pngPath]);
  exitCode := fpsystem(cmd);
  Result := (exitCode = 0);
end;

// ---------------- Main: construir AVL desde draft.json ----------------
function GenerateAVLReportFromDrafts(const outputFolder: string): Boolean;
var
  outFolder, dotPath, pngPath: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  mailsArr: TJSONArray;
  i: Integer;
  itemObj: TJSONObject;
  idInt: Integer;
  sId: string;
  remitente, destinatario, asunto, mensaje, estado: string;
  nodeLabel: string;
  fOk: Boolean;
begin
  Result := False;
  rootAVL := nil;

  if Trim(outputFolder) = '' then
    outFolder := Trim(current_user_username) + '-Reports'
  else
    outFolder := outputFolder;

  try
    if not DirectoryExists(outFolder) then
      ForceDirectories(outFolder);
  except
    Exit;
  end;

  dotPath := IncludeTrailingPathDelimiter(outFolder) + 'avl_drafts.dot';
  pngPath := IncludeTrailingPathDelimiter(outFolder) + 'avl_drafts.png';

  jsonData := LoadJSONDataFile(json_file_drafts);
  if jsonData = nil then
  begin
    jsonData := LoadJSONDataFile('draft.json');
    if jsonData = nil then
    begin
      GenerateDOTFile(dotPath);
      fOk := RunGraphviz(dotPath, pngPath);
      Result := fOk;
      FreeAVL(rootAVL);
      Exit;
    end;
  end;

  if jsonData.JSONType = jtObject then
  begin
    jsonObj := TJSONObject(jsonData);
    if jsonObj.IndexOfName('mails') <> -1 then
      mailsArr := jsonObj.Arrays['mails']
    else if jsonObj.IndexOfName('drafts') <> -1 then
      mailsArr := jsonObj.Arrays['drafts']
    else
      mailsArr := nil;
  end
  else if jsonData.JSONType = jtArray then
    mailsArr := TJSONArray(jsonData)
  else
    mailsArr := nil;

  if (mailsArr = nil) or (mailsArr.Count = 0) then
  begin
    GenerateDOTFile(dotPath);
    fOk := RunGraphviz(dotPath, pngPath);
    Result := fOk;
    if Assigned(jsonData) then jsonData.Free;
    FreeAVL(rootAVL);
    Exit;
  end;

  for i := 0 to mailsArr.Count - 1 do
  begin
    if not (mailsArr.Items[i] is TJSONObject) then Continue;
    itemObj := mailsArr.Objects[i];

    idInt := 0;
    if itemObj.IndexOfName('id') <> -1 then
    begin
      try
        idInt := itemObj.Integers['id'];
      except
        sId := itemObj.Get('id', '');
        idInt := StrToIntDef(sId, 0);
      end;
    end;

    remitente := itemObj.Get('remitente', itemObj.Get('sender', ''));
    destinatario := itemObj.Get('destinatario', itemObj.Get('receiver', ''));
    asunto := itemObj.Get('asunto', itemObj.Get('subject', ''));
    mensaje := itemObj.Get('mensaje', itemObj.Get('message', ''));
    estado := itemObj.Get('estado', itemObj.Get('state', ''));

    if Length(mensaje) > 200 then
      mensaje := Copy(mensaje, 1, 197) + '...';

    // 👇 Aquí forzamos los saltos de línea para que se vean uno debajo de otro
    nodeLabel := Format('ID: %d\nRemitente: %s\nDestinatario: %s\nAsunto: %s\nEstado: %s\nMensaje: %s',
                        [idInt, remitente, destinatario, asunto, estado, mensaje]);

    rootAVL := AVLInsert(rootAVL, idInt, nodeLabel);
  end;

  fOk := GenerateDOTFile(dotPath);
  if not fOk then
  begin
    FreeAVL(rootAVL);
    if Assigned(jsonData) then jsonData.Free;
    Exit;
  end;

  fOk := RunGraphviz(dotPath, pngPath);
  if not fOk then
  begin
    FreeAVL(rootAVL);
    if Assigned(jsonData) then jsonData.Free;
    Exit;
  end;

  Result := True;

  FreeAVL(rootAVL);
  rootAVL := nil;
  if Assigned(jsonData) then jsonData.Free;
end;

end.
