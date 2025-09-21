unit graphviz;

{$mode objfpc}{$H+}

interface

uses bst, SysUtils;

procedure ExportToDOT(Root: PNode; const FileName: String);

implementation

procedure WriteDOTNode(var F: Text; Node: PNode);
begin
  if Node = nil then Exit;

  WriteLn(F, '  ', Node^.id, ' [label="', Node^.id, '\n',
           Node^.first_name, ' ', Node^.last_name, '"];');

  if Node^.left <> nil then
  begin
    WriteLn(F, '  ', Node^.id, ' -> ', Node^.left^.id, ';');
    WriteDOTNode(F, Node^.left);
  end;

  if Node^.right <> nil then
  begin
    WriteLn(F, '  ', Node^.id, ' -> ', Node^.right^.id, ';');
    WriteDOTNode(F, Node^.right);
  end;
end;

procedure ExportToDOT(Root: PNode; const FileName: String);
var
  F: Text;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  WriteLn(F, 'digraph BST {');
  WriteLn(F, '  node [shape=box, style=filled, color=lightblue];');
  WriteDOTNode(F, Root);
  WriteLn(F, '}');
  CloseFile(F);
end;

end.
