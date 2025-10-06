unit btree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

/// Genera un reporte (DOT + PNG) del BTree construido con los favoritos (favorites.json)
function GenerateBTreeReportFromFavorites(const outputFolder: string): Boolean;

implementation

uses
  fpjson, jsonparser, Process, Unix,
  variables, jsonTools,
  interfaceTools;

type
  PBTreeNode = ^TBTreeNode;
  TBTreeNode = record
    id: Integer;
    txt: string;
    left, right: PBTreeNode;
  end;

var
  rootBTree: PBTreeNode = nil;

// ---------------- BTree helpers ----------------
function CreateBTreeNode(aid: Integer; const alabel: string): PBTreeNode;
var
  n: PBTreeNode;
begin
  New(n);
  n^.id := aid;
  n^.txt := alabel;
  n^.left := nil;
  n^.right := nil;
  Result := n;
end;

function BTreeInsert(node: PBTreeNode; aid: Integer; const alabel: string): PBTreeNode;
begin
  if node = nil then
  begin
    Result := CreateBTreeNode(aid, alabel);
    Exit;
  end;

  if aid < node^.id then
    node^.left := BTreeInsert(node^.left, aid, alabel)
  else if aid > node^.id then
    node^.right := BTreeInsert(node^.right, aid, alabel)
  else
  begin
    node^.txt := alabel; // actualizar si existe
    Result := node;
    Exit;
  end;

  Result := node;
end;

procedure FreeBTree(node: PBTreeNode);
begin
  if node = nil then Exit;
  FreeBTree(node^.left);
  FreeBTree(node^.right);
  Dispose(node);
end;

// ---------------- DOT generation ----------------
procedure EscapeAndWriteLabelBTree(const s: string; out outS: string);
begin
  outS := StringReplace(s, '\', '\\', [rfReplaceAll]);
  outS := StringReplace(outS, '"', '\"', [rfReplaceAll]);
  outS := StringReplace(outS, sLineBreak, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #13#10, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #13, '\n', [rfReplaceAll]);
  outS := StringReplace(outS, #10, '\n', [rfReplaceAll]);
end;

procedure WriteDotNodeBTree(var f: Text; node: PBTreeNode);
var lab: string;
begin
  if node = nil then Exit;
  EscapeAndWriteLabelBTree(node^.txt, lab);
  Writeln(f, Format('  "n%d" [label="%s", shape=box, style=filled, fillcolor=lightblue, fontname="Helvetica", fontsize=10];', [node^.id, lab]));
  if node^.left <> nil then
    Writeln(f, Format('  "n%d" -> "n%d";', [node^.id, node^.left^.id]));
  if node^.right <> nil then
    Writeln(f, Format('  "n%d" -> "n%d";', [node^.id, node^.right^.id]));
  WriteDotNodeBTree(f, node^.left);
  WriteDotNodeBTree(f, node^.right);
end;

function GenerateDOTFileBTree(const dotPath: string): Boolean;
var f: Text;
begin
  Result := False;
  try
    AssignFile(f, dotPath);
    Rewrite(f);
    try
      Writeln(f, 'digraph BTree {');
      Writeln(f, '  rankdir=TB;');
      Writeln(f, '  node [shape=box, style=filled, fillcolor=lightyellow, fontname="Helvetica"];');
      if rootBTree = nil then
        Writeln(f, '  empty [label="(vacio)"];')
      else
        WriteDotNodeBTree(f, rootBTree);
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
function RunGraphvizBTree(const dotPath, pngPath: string): Boolean;
var
  cmd: string;
  exitCode: LongInt;
begin
  Result := False;
  cmd := Format('dot -Tpng "%s" -o "%s"', [dotPath, pngPath]);
  exitCode := fpsystem(cmd);
  Result := (exitCode = 0);
end;

// ---------------- Main: construir BTree desde favorites.json ----------------
function GenerateBTreeReportFromFavorites(const outputFolder: string): Boolean;
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
  rootBTree := nil;

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

  dotPath := IncludeTrailingPathDelimiter(outFolder) + 'btree_favorites.dot';
  pngPath := IncludeTrailingPathDelimiter(outFolder) + 'btree_favorites.png';

  jsonData := LoadJSONDataFile('favorites.json');
  if jsonData = nil then
  begin
    GenerateDOTFileBTree(dotPath);
    fOk := RunGraphvizBTree(dotPath, pngPath);
    Result := fOk;
    FreeBTree(rootBTree);
    Exit;
  end;

  if jsonData.JSONType = jtObject then
  begin
    jsonObj := TJSONObject(jsonData);
    if jsonObj.IndexOfName('mails') <> -1 then
      mailsArr := jsonObj.Arrays['mails']
    else
      mailsArr := nil;
  end
  else if jsonData.JSONType = jtArray then
    mailsArr := TJSONArray(jsonData)
  else
    mailsArr := nil;

  if (mailsArr = nil) or (mailsArr.Count = 0) then
  begin
    GenerateDOTFileBTree(dotPath);
    fOk := RunGraphvizBTree(dotPath, pngPath);
    Result := fOk;
    if Assigned(jsonData) then jsonData.Free;
    FreeBTree(rootBTree);
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

    nodeLabel := Format('ID: %d\nRemitente: %s\nDestinatario: %s\nAsunto: %s\nEstado: %s\nMensaje: %s',
                        [idInt, remitente, destinatario, asunto, estado, mensaje]);

    rootBTree := BTreeInsert(rootBTree, idInt, nodeLabel);
  end;

  fOk := GenerateDOTFileBTree(dotPath);
  if not fOk then
  begin
    FreeBTree(rootBTree);
    if Assigned(jsonData) then jsonData.Free;
    Exit;
  end;

  fOk := RunGraphvizBTree(dotPath, pngPath);
  if not fOk then
  begin
    FreeBTree(rootBTree);
    if Assigned(jsonData) then jsonData.Free;
    Exit;
  end;

  Result := True;

  FreeBTree(rootBTree);
  rootBTree := nil;
  if Assigned(jsonData) then jsonData.Free;
end;

end.

