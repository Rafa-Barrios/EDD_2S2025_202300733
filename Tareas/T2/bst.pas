unit bst;

{$mode objfpc}{$H+}

interface

type
  PNode = ^TNode;
  TNode = record
    id: Integer;
    first_name, last_name, email: String;
    left, right: PNode;
  end;

procedure InsertNode(var Root: PNode; id: Integer; fname, lname, email: String);

implementation

procedure InsertNode(var Root: PNode; id: Integer; fname, lname, email: String);
var
  NewNode: PNode;
begin
  if Root = nil then
  begin
    New(NewNode);
    NewNode^.id := id;
    NewNode^.first_name := fname;
    NewNode^.last_name := lname;
    NewNode^.email := email;
    NewNode^.left := nil;
    NewNode^.right := nil;
    Root := NewNode;
  end
  else if id < Root^.id then
    InsertNode(Root^.left, id, fname, lname, email)
  else
    InsertNode(Root^.right, id, fname, lname, email);
end;

end.
