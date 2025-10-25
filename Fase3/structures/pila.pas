unit pila;

{$MODE DELPHI}

interface

type
    PMailStackNode = ^TMailStackNode;
    TMailStackNode = record
        id: string;
        sender: string;
        recipient: string;
        subject: string;
        message: string;
        date: string;
        Next: PMailStackNode;
    end;

procedure Stack_Push(id, sender, recipient, subject, message, date: string);
function  Stack_Pop: PMailStackNode;
function  Stack_IsEmpty: Boolean;
procedure Stack_Clear;
procedure Stack_PrintToConsole;

function  Stack_Peek: PMailStackNode; // 🔹 Ver el tope sin eliminar

implementation

uses
    SysUtils;

var
    Top: PMailStackNode = nil;

// ------------------------------
// Insertar arriba de la pila (push)
// ------------------------------
procedure Stack_Push(id, sender, recipient, subject, message, date: string);
var
    NewNode: PMailStackNode;
begin
    New(NewNode);
    NewNode^.id := Trim(id);
    NewNode^.sender := Trim(sender);
    NewNode^.recipient := Trim(recipient);
    NewNode^.subject := Trim(subject);
    NewNode^.message := Trim(message);
    NewNode^.date := Trim(date);

    NewNode^.Next := Top;  // el nuevo nodo apunta al anterior tope
    Top := NewNode;        // ahora el nuevo nodo es el tope
end;

// ------------------------------
// Sacar el elemento superior (pop)
// ------------------------------
function Stack_Pop: PMailStackNode;
var
    Temp: PMailStackNode;
begin
    if Top = nil then
    begin
        Result := nil;
        Exit;
    end;

    Temp := Top;
    Top := Top^.Next;  // el nuevo tope es el siguiente nodo
    Temp^.Next := nil; // desconectar
    Result := Temp;    // devolvemos el nodo desapilado
end;

// ------------------------------
// Revisar si la pila está vacía
// ------------------------------
function Stack_IsEmpty: Boolean;
begin
    Result := (Top = nil);
end;

// ------------------------------
// Limpiar toda la pila
// ------------------------------
procedure Stack_Clear;
var
    Temp: PMailStackNode;
begin
    while Top <> nil do
    begin
        Temp := Top;
        Top := Top^.Next;
        Dispose(Temp);
    end;
end;

// ------------------------------
// Imprimir pila en consola
// ------------------------------
procedure Stack_PrintToConsole;
var
    Current: PMailStackNode;
begin
    if Top = nil then
    begin
        Writeln('La pila de la papelera está vacía.');
        Exit;
    end;

    Current := Top;
    Writeln('--- Contenido de la Pila (Papelera) ---');
    while Current <> nil do
    begin
        Writeln('ID: ', Current^.id);
        Writeln('Remitente: ', Current^.sender);
        Writeln('Destinatario: ', Current^.recipient);
        Writeln('Asunto: ', Current^.subject);
        Writeln('Mensaje: ', Current^.message);
        Writeln('Fecha: ', Current^.date);
        Writeln('-------------------');
        Current := Current^.Next;
    end;
end;

// ------------------------------
// Ver el nodo en el tope (peek)
// ------------------------------
function Stack_Peek: PMailStackNode;
begin
    Result := Top;
end;

end.
