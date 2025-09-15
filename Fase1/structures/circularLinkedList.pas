unit circularLinkedList;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  PCNode = ^TCNode;
  TCNode = record
    id: string;          // ID del contacto
    nombre: string;      // Nombre del contacto
    email: string;       // Correo del contacto (clave principal)
    telefono: string;    // Teléfono del contacto
    siguiente: PCNode;   // Apunta al siguiente nodo
  end;

// Lista global opcional
var
  CL_Head: PCNode = nil;

// ---- Operaciones básicas ----
procedure CL_Insert(const id, nombre, email: string); 
function CL_Search(const email: string): PCNode;
procedure CL_Display;
procedure CL_Clear;

// ---- Operaciones para listas por usuario ----
procedure CL_InsertToList(var head: PCNode; const id, nombre, email, telefono: string);
function CL_SearchInList(head: PCNode; const email: string): PCNode;
procedure CL_DisplayFromList(head: PCNode);
procedure CL_ClearList(var head: PCNode);

// ---- Generar DOT ----
function CL_GenerateDot(head: PCNode): string;

implementation

// =====================
// Escapar caracteres para DOT
// =====================
function EscapeDotString(const S: string): string;
var
  Res: string;
  i: Integer;
begin
  Res := '';
  for i := 1 to Length(S) do
  begin
    case S[i] of
      '"': Res := Res + '\"';
      '\': Res := Res + '\\';
      '|': Res := Res + '\|';
      '{': Res := Res + '\{';
      '}': Res := Res + '\}';
      #10, #13: Res := Res + '\n';
    else
      Res := Res + S[i];
    end;
  end;
  Result := Res;
end;

// =====================
// Funciones existentes
// =====================

procedure CL_Insert(const id, nombre, email: string);
var
  nuevo, temp: PCNode;
begin
  if CL_Search(email) <> nil then Exit;

  New(nuevo);
  nuevo^.id := id;
  nuevo^.nombre := nombre;
  nuevo^.email := email;
  nuevo^.telefono := '';
  
  if CL_Head = nil then
  begin
    CL_Head := nuevo;
    nuevo^.siguiente := CL_Head;
  end
  else
  begin
    temp := CL_Head;
    while temp^.siguiente <> CL_Head do
      temp := temp^.siguiente;

    temp^.siguiente := nuevo;
    nuevo^.siguiente := CL_Head;
  end;
end;

function CL_Search(const email: string): PCNode;
var
  temp: PCNode;
begin
  Result := nil;
  if CL_Head = nil then Exit;
  temp := CL_Head;
  repeat
    if SameText(temp^.email, email) then
    begin
      Result := temp;
      Exit;
    end;
    temp := temp^.siguiente;
  until temp = CL_Head;
end;

procedure CL_Display;
var
  temp: PCNode;
begin
  if CL_Head = nil then
  begin
    Writeln('No hay contactos en la lista circular.');
    Exit;
  end;

  temp := CL_Head;
  Writeln('--- Lista de contactos ---');
  repeat
    Writeln('ID: ', temp^.id, ' | Nombre: ', temp^.nombre, ' | Email: ', temp^.email);
    temp := temp^.siguiente;
  until temp = CL_Head;
end;

procedure CL_Clear;
var
  temp, aux: PCNode;
begin
  if CL_Head = nil then Exit;
  temp := CL_Head^.siguiente;
  while temp <> CL_Head do
  begin
    aux := temp;
    temp := temp^.siguiente;
    Dispose(aux);
  end;
  Dispose(CL_Head);
  CL_Head := nil;
end;

// =====================
// Funciones para listas por usuario
// =====================

procedure CL_InsertToList(var head: PCNode; const id, nombre, email, telefono: string);
var
  nuevo, temp: PCNode;
begin
  if head = nil then
  begin
    New(nuevo);
    nuevo^.id := id;
    nuevo^.nombre := nombre;
    nuevo^.email := email;
    nuevo^.telefono := telefono;
    nuevo^.siguiente := nuevo;
    head := nuevo;
    Exit;
  end;

  temp := head;
  repeat
    if SameText(temp^.email, email) then Exit; // ya existe
    temp := temp^.siguiente;
  until temp = head;

  New(nuevo);
  nuevo^.id := id;
  nuevo^.nombre := nombre;
  nuevo^.email := email;
  nuevo^.telefono := telefono;
  nuevo^.siguiente := head;

  temp := head;
  while temp^.siguiente <> head do
    temp := temp^.siguiente;

  temp^.siguiente := nuevo;
end;

function CL_SearchInList(head: PCNode; const email: string): PCNode;
var
  temp: PCNode;
begin
  Result := nil;
  if head = nil then Exit;
  temp := head;
  repeat
    if SameText(temp^.email, email) then
    begin
      Result := temp;
      Exit;
    end;
    temp := temp^.siguiente;
  until temp = head;
end;

procedure CL_DisplayFromList(head: PCNode);
var
  temp: PCNode;
begin
  if head = nil then
  begin
    Writeln('No hay contactos en esta lista.');
    Exit;
  end;

  temp := head;
  Writeln('--- Contactos del usuario ---');
  repeat
    Writeln('ID: ', temp^.id, ' | Nombre: ', temp^.nombre, ' | Email: ', temp^.email, ' | Tel: ', temp^.telefono);
    temp := temp^.siguiente;
  until temp = head;
end;

procedure CL_ClearList(var head: PCNode);
var
  temp, aux: PCNode;
begin
  if head = nil then Exit;
  temp := head^.siguiente;
  while temp <> head do
  begin
    aux := temp;
    temp := temp^.siguiente;
    Dispose(aux);
  end;
  Dispose(head);
  head := nil;
end;

// =====================
// Generar DOT (lista circular)
// =====================
function CL_GenerateDot(head: PCNode): string;
var
  SL: TStringList;
  temp: PCNode;
  counter: Integer;
  NodeName, NextName: string;
begin
  SL := TStringList.Create;
  SL.Add('digraph ListaCircular {');
  SL.Add('  rankdir=LR;');
  SL.Add('  nodesep=0.5;');
  SL.Add('  node [shape=record, style=filled, fillcolor=lightgreen];');
  SL.Add('');

  if head = nil then
  begin
    SL.Add('  null [label="VACÍA", shape=plaintext];');
  end
  else
  begin
    counter := 0;
    temp := head;
    repeat
      NodeName := Format('node%d', [counter]);
      SL.Add(Format('  %s [label="{ID: %s\lNombre: %s\lEmail: %s\lTel: %s\l}"];',
        [NodeName,
         EscapeDotString(temp^.id),
         EscapeDotString(temp^.nombre),
         EscapeDotString(temp^.email),
         EscapeDotString(temp^.telefono)]));

      // Conexión al siguiente
      NextName := Format('node%d', [counter + 1]);
      SL.Add(Format('  %s -> %s;', [NodeName, NextName]));

      Inc(counter);
      temp := temp^.siguiente;
    until temp = head;

    // Conexión final para cerrar círculo
    SL.Add(Format('  node%d -> node0;', [counter - 1]));
  end;

  SL.Add('}');
  Result := SL.Text;
  SL.Free;
end;

end.