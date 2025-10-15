unit btree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function GenerateBTreeReportFromFavorites(const outputFolder: string): Boolean;

implementation

uses
  fpjson, jsonparser, Process, Unix,
  variables, jsonTools,
  filesTools, 
  interfaceTools;

const
  M = 3;

type
  PBNode = ^TBNode;
  TBNode = record
    n: Integer;
    keys: array[1..M*2-1] of Integer;
    labels: array[1..M*2-1] of string;
    children: array[1..M*2] of PBNode;
    leaf: Boolean;
  end;

var
  rootB: PBNode = nil;

function CreateNode(leaf: Boolean): PBNode;
var
  i: Integer;
begin
  New(Result);
  Result^.n := 0;
  Result^.leaf := leaf;
  for i := 1 to M*2 do
    Result^.children[i] := nil;
end;

procedure SplitChild(x: PBNode; i: Integer; y: PBNode);
var
  z: PBNode;
  j: Integer;
begin
  z := CreateNode(y^.leaf);
  z^.n := M - 1;
  for j := 1 to M - 1 do
  begin
    z^.keys[j] := y^.keys[j + M];
    z^.labels[j] := y^.labels[j + M];
  end;
  if not y^.leaf then
    for j := 1 to M do
      z^.children[j] := y^.children[j + M];
  y^.n := M - 1;

  for j := x^.n + 1 downto i + 1 do
    x^.children[j + 1] := x^.children[j];
  x^.children[i + 1] := z;

  for j := x^.n downto i do
  begin
    x^.keys[j + 1] := x^.keys[j];
    x^.labels[j + 1] := x^.labels[j];
  end;

  x^.keys[i] := y^.keys[M];
  x^.labels[i] := y^.labels[M];
  Inc(x^.n);
end;

procedure InsertNonFull(x: PBNode; k: Integer; const lbl: string);
var
  i: Integer;
begin
  i := x^.n;
  if x^.leaf then
  begin
    while (i >= 1) and (k < x^.keys[i]) do
    begin
      x^.keys[i + 1] := x^.keys[i];
      x^.labels[i + 1] := x^.labels[i];
      Dec(i);
    end;
    x^.keys[i + 1] := k;
    x^.labels[i + 1] := lbl;
    Inc(x^.n);
  end
  else
  begin
    while (i >= 1) and (k < x^.keys[i]) do
      Dec(i);
    Inc(i);
    if x^.children[i]^.n = 2 * M - 1 then
    begin
      SplitChild(x, i, x^.children[i]);
      if k > x^.keys[i] then
        Inc(i);
    end;
    InsertNonFull(x^.children[i], k, lbl);
  end;
end;

procedure BTreeInsert(k: Integer; const lbl: string);
var
  r, s: PBNode;
begin
  if rootB = nil then
  begin
    rootB := CreateNode(True);
    rootB^.keys[1] := k;
    rootB^.labels[1] := lbl;
    rootB^.n := 1;
    Exit;
  end;

  r := rootB;
  if r^.n = 2 * M - 1 then
  begin
    s := CreateNode(False);
    rootB := s;
    s^.children[1] := r;
    SplitChild(s, 1, r);
    InsertNonFull(s, k, lbl);
  end
  else
    InsertNonFull(r, k, lbl);
end;

procedure FreeBTree(node: PBNode);
var
  i: Integer;
begin
  if node = nil then Exit;
  if not node^.leaf then
    for i := 1 to node^.n + 1 do
      FreeBTree(node^.children[i]);
  Dispose(node);
end;

// ---------------- DOT con TStringList (como avltree) ----------------
procedure WriteDotNode(dot: TStringList; node: PBNode; parentID: string; var counter: Integer);
var
  i: Integer;
  nodeID, cleanLabel: string;
begin
  if node = nil then Exit;
  Inc(counter);
  nodeID := 'node' + IntToStr(counter);

  cleanLabel := '';
  for i := 1 to node^.n do
  begin
    cleanLabel += StringReplace(node^.labels[i], '"', '\"', [rfReplaceAll]);
    if i < node^.n then
      cleanLabel += '\n----------------\n';
  end;

  dot.Add(Format('  %s [label="%s", shape=box, style=filled, fillcolor=lightgoldenrod, fontsize=10];',
                 [nodeID, cleanLabel]));

  if parentID <> '' then
    dot.Add(Format('  %s -> %s;', [parentID, nodeID]));

  if not node^.leaf then
    for i := 1 to node^.n + 1 do
      WriteDotNode(dot, node^.children[i], nodeID, counter);
end;

function GenerateDOTFile(const dotPath: string): Boolean;
var
  dot: TStringList;
  counter: Integer;
begin
  Result := False;
  dot := TStringList.Create;
  try
    dot.Add('digraph BTree {');
    dot.Add('  rankdir=TB;');
    dot.Add('  node [shape=box, style=filled, fillcolor=lightyellow, fontname="Helvetica"];');

    counter := 0;
    if rootB = nil then
      dot.Add('  empty [label="(vacio)"];')
    else
      WriteDotNode(dot, rootB, '', counter);

    dot.Add('}');
    dot.SaveToFile(dotPath);
    Result := True;
  finally
    dot.Free;
  end;
end;

function RunGraphviz(const dotPath, pngPath: string): Boolean;
var
  cmd: string;
begin
  cmd := Format('dot -Tpng "%s" -o "%s"', [dotPath, pngPath]);
  Result := (fpsystem(cmd) = 0);
end;

// ---------------- Genera reporte desde favorites.json ----------------
function GenerateBTreeReportFromFavorites(const outputFolder: string): Boolean;
var
  outFolder, dotPath, pngPath: string;
  jsonData: TJSONData;
  jsonObj: TJSONObject;
  mailsArr: TJSONArray;
  i: Integer;
  itemObj: TJSONObject;
  idInt: Integer;
  remitente, destinatario, asunto, mensaje, estado: string;
  nodeLabel: string;
  fOk: Boolean;
begin
  Result := False;
  rootB := nil;

  if Trim(outputFolder) = '' then
    outFolder := Trim(current_user_username) + '-Reports'
  else
    outFolder := outputFolder;

  if not DirectoryExists(outFolder) then
    ForceDirectories(outFolder);

  dotPath := IncludeTrailingPathDelimiter(outFolder) + 'btree_favorites.dot';
  pngPath := IncludeTrailingPathDelimiter(outFolder) + 'btree_favorites.png';

  jsonData := LoadJSONDataFile(json_file_favorites);
  if jsonData = nil then
  begin
    GenerateDOTFile(dotPath);
    fOk := RunGraphviz(dotPath, pngPath);
    Result := fOk;
    Exit;
  end;

  if jsonData.JSONType = jtObject then
  begin
    jsonObj := TJSONObject(jsonData);
    if jsonObj.IndexOfName('mails') <> -1 then
      mailsArr := jsonObj.Arrays['mails']
    else if jsonObj.IndexOfName('sent') <> -1 then
      mailsArr := jsonObj.Arrays['sent']
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
    Exit;
  end;

  for i := 0 to mailsArr.Count - 1 do
  begin
    if not (mailsArr.Items[i] is TJSONObject) then Continue;
    itemObj := mailsArr.Objects[i];

    idInt := itemObj.Get('id', i + 1);
    remitente := itemObj.Get('remitente', itemObj.Get('sender', ''));
    destinatario := itemObj.Get('destinatario', itemObj.Get('receiver', ''));
    asunto := itemObj.Get('asunto', itemObj.Get('subject', ''));
    mensaje := itemObj.Get('mensaje', itemObj.Get('message', ''));
    estado := itemObj.Get('estado', itemObj.Get('state', ''));

    if Length(mensaje) > 200 then
      mensaje := Copy(mensaje, 1, 197) + '...';

    nodeLabel := Format('ID: %d\nRemitente: %s\nDestinatario: %s\nAsunto: %s\nEstado: %s\nMensaje:\n%s',
                        [idInt, remitente, destinatario, asunto, estado, mensaje]);

    BTreeInsert(idInt, nodeLabel);
  end;

  fOk := GenerateDOTFile(dotPath);
  if fOk then
    fOk := RunGraphviz(dotPath, pngPath);

  Result := fOk;
  FreeBTree(rootB);
  rootB := nil;
  if Assigned(jsonData) then jsonData.Free;
end;

end.
