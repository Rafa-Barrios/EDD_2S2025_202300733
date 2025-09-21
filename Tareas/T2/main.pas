program T2_BST;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser,
  bst, jsonLoader, graphviz, Process;

var
  Root: PNode;
begin
  Root := nil;
  // Cargar JSON e insertar en el árbol
  LoadFromJSON('datos.json', Root);

  // Exportar a Graphviz
  ExportToDOT(Root, 'bst.dot');

    // Llamar a graphviz (requiere tener instalado graphviz)
  if ExecuteProcess('/usr/bin/dot', ['-Tpng', 'bst.dot', '-o', 'bst.png']) = 0 then
    WriteLn('Árbol generado en bst.png')
  else
    WriteLn('Error al ejecutar Graphviz.');
end.
