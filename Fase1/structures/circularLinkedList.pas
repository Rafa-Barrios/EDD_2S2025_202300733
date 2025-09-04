unit circularLinkedList;

{$mode objfpc}{$H+}

interface

uses
    SysUtils;

type
    PCNode = ^TCNode;
    TCNode = record
        id: string;          // ID del contacto
        nombre: string;      // Nombre del contacto
        email: string;       // Correo del contacto (clave principal)
        siguiente: PCNode;   // Apunta al siguiente nodo
    end;

var
    CL_Head: PCNode = nil;   // Puntero al inicio de la lista circular

// ---- Operaciones básicas ----
procedure CL_Insert(const id, nombre, email: string); // insertar contacto
function  CL_Search(const email: string): PCNode;      // buscar contacto
procedure CL_Display;                                  // mostrar lista
procedure CL_Clear;                                    // limpiar lista

implementation

// Insertar un contacto en la lista circular
procedure CL_Insert(const id, nombre, email: string);
var
    nuevo, temp: PCNode;
begin
    // Evitar duplicados
    if CL_Search(email) <> nil then
    begin
        Writeln('Contacto ya existe en la lista circular: ', email);
        Exit;
    end;

    New(nuevo);
    nuevo^.id := id;
    nuevo^.nombre := nombre;
    nuevo^.email := email;

    if CL_Head = nil then
    begin
        CL_Head := nuevo;
        nuevo^.siguiente := CL_Head;  // circular
    end
    else
    begin
        temp := CL_Head;
        while temp^.siguiente <> CL_Head do
            temp := temp^.siguiente;

        temp^.siguiente := nuevo;
        nuevo^.siguiente := CL_Head;
    end;

    Writeln('Contacto agregado: ', nombre, ' (', email, ')');
end;

// Buscar un contacto por correo
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

// Mostrar todos los contactos en la lista
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

// Liberar memoria y vaciar lista
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

end.