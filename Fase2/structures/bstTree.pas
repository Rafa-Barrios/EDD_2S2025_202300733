unit bstTree;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes, fpjson, jsonparser, variables, filesTools;

type
  PBSTNode = ^TBSTNode;
  TBSTNode = record
    comunidad: string;
    fecha: string;
    mensajes: Integer;
    left, right: PBSTNode;
  end;

var
  BST_Root: PBSTNode = nil;

// Procedimientos principales
procedure BST_Insert(var root: PBSTNode; comunidad, fecha: string; mensajes: Integer);
procedure BST_LoadFromJSON;
function BST_GenerateDot(): string;
procedure BST_GenerateReport;

implementation

// --------------------------- INSERTAR ---------------------------
procedure BST_Insert(var root: PBSTNode; comunidad, fecha: string; mensajes: Integer);
var
  newNode: PBSTNode;
begin
  if root = nil then
  begin
    New(newNode);
    newNode^.comunidad := comunidad;
    newNode^.fecha := fecha;
    newNode^.mensajes := mensajes;
    newNode^.left := nil;
    newNode^.right := nil;
    root := newNode;
  end
  else if mensajes < root^.mensajes then
    BST_Insert(root^.left, comunidad, fecha, mensajes)
  else
    BST_Insert(root^.right, comunidad, fecha, mensajes);
end;

// --------------------------- CARGAR DESDE JSON ---------------------------
procedure BST_LoadFromJSON;
var
  JSONData: TJSONData;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  comunidad, fecha: string;
  mensajes: Integer;
  i: Integer;
  JSONString: TStringList;
begin
  BST_Root := nil;

  if not FileExists(json_file_communities) then
  begin
    WriteLn('ERROR: No se encontró el archivo de comunidades JSON.');
    Exit;
  end;

  JSONString := TStringList.Create;
  try
    JSONString.LoadFromFile(json_file_communities);
    JSONData := GetJSON(JSONString.Text);

    JSONArray := TJSONObject(JSONData).Arrays['comunidades'];
    for i := 0 to JSONArray.Count - 1 do
    begin
      JSONObject := JSONArray.Objects[i];
      comunidad := JSONObject.Strings['Comunidad'];
      fecha := JSONObject.Strings['fecha'];
      mensajes := JSONObject.Integers['mensajes'];
      BST_Insert(BST_Root, comunidad, fecha, mensajes);
    end;

  finally
    JSONString.Free;
  end;
end;

// --------------------------- GENERAR DOT ---------------------------
procedure BST_GenerateDotNodes(root: PBSTNode; var dot: TStringList);
begin
  if root = nil then
    Exit;

  dot.Add(Format('"%p" [label="Comunidad: %s\nFecha: %s\nMensajes: %d"];',
    [Pointer(root), root^.comunidad, root^.fecha, root^.mensajes]));

  if root^.left <> nil then
  begin
    dot.Add(Format('"%p" -> "%p";', [Pointer(root), Pointer(root^.left)]));
    BST_GenerateDotNodes(root^.left, dot);
  end;

  if root^.right <> nil then
  begin
    dot.Add(Format('"%p" -> "%p";', [Pointer(root), Pointer(root^.right)]));
    BST_GenerateDotNodes(root^.right, dot);
  end;
end;

function BST_GenerateDot(): string;
var
  dot: TStringList;
begin
  dot := TStringList.Create;
  try
    dot.Add('digraph BST {');
    dot.Add('  node [shape=box, style=filled, color=lightblue, fontname="Arial"];');
    dot.Add('  rankdir=TB;');

    if BST_Root <> nil then
      BST_GenerateDotNodes(BST_Root, dot)
    else
      dot.Add('"vacio" [label="No hay comunidades cargadas"];');

    dot.Add('}');
    Result := dot.Text;
  finally
    dot.Free;
  end;
end;

// --------------------------- GENERAR REPORTE (DOT + PNG) ---------------------------
procedure BST_GenerateReport;
var
  dotStr: string;
begin
  BST_LoadFromJSON; // Cargar el JSON antes de generar el reporte
  dotStr := BST_GenerateDot;
  filesTools.GenerateReports('bst_comunidades', 'Root-Reports', dotStr);
end;

end.
