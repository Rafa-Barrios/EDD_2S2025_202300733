unit cola;

{$MODE DELPHI}

interface

type
    PMailQueueNode = ^TMailQueueNode;
    TMailQueueNode = record
        id: string;
        sender: string;
        recipient: string;
        subject: string;
        message: string;         // cuerpo completo
        dateScheduled: string;   // fecha programada (solo dato informativo)
        Next: PMailQueueNode;
    end;

procedure Queue_Enqueue(id, sender, recipient, subject, message, dateScheduled: string);
function  Queue_Dequeue: PMailQueueNode;
function  Queue_IsEmpty: Boolean;
procedure Queue_Clear;
procedure Queue_PrintToConsole;

function  Queue_Peek: PMailQueueNode; // ver el primero sin eliminar
function  Queue_Count: Integer;       // contar elementos

// 🔹 Exportar la cola como arreglo dinámico de punteros
procedure Queue_ToArray(var Arr: array of PMailQueueNode);

implementation

uses
    SysUtils;

var
    Front, Rear: PMailQueueNode; // frente y final de la cola

// ------------------------------
// Inicialización
// ------------------------------
procedure InitQueue;
begin
    Front := nil;
    Rear := nil;
end;

// ------------------------------
// Encolar (insertar al final)
// ------------------------------
procedure Queue_Enqueue(id, sender, recipient, subject, message, dateScheduled: string);
var
    NewNode: PMailQueueNode;
begin
    New(NewNode);
    NewNode^.id := Trim(id);
    NewNode^.sender := Trim(sender);
    NewNode^.recipient := Trim(recipient);
    NewNode^.subject := Trim(subject);
    NewNode^.message := Trim(message);
    NewNode^.dateScheduled := Trim(dateScheduled);
    NewNode^.Next := nil;

    if Rear = nil then
    begin
        // cola vacía, el nuevo nodo es frente y final
        Front := NewNode;
        Rear := NewNode;
    end
    else
    begin
        Rear^.Next := NewNode; // enlazar al final
        Rear := NewNode;       // actualizar puntero final
    end;
end;

// ------------------------------
// Desencolar (sacar del frente)
// ------------------------------
function Queue_Dequeue: PMailQueueNode;
begin
    if Front = nil then
    begin
        Result := nil;
        Exit;
    end;

    Result := Front;
    Front := Front^.Next;

    if Front = nil then
        Rear := nil; // si se vació, rear también queda nil

    Result^.Next := nil; // desconectar del resto
end;

// ------------------------------
// Revisar si la cola está vacía
// ------------------------------
function Queue_IsEmpty: Boolean;
begin
    Result := (Front = nil);
end;

// ------------------------------
// Limpiar toda la cola
// ------------------------------
procedure Queue_Clear;
var
    Temp: PMailQueueNode;
begin
    while Front <> nil do
    begin
        Temp := Front;
        Front := Front^.Next;
        Dispose(Temp); // liberar memoria
    end;
    Rear := nil;
end;

// ------------------------------
// Imprimir cola en consola
// ------------------------------
procedure Queue_PrintToConsole;
var
    Current: PMailQueueNode;
begin
    if Front = nil then
    begin
        Writeln('La cola de mensajes programados está vacía.');
        Exit;
    end;

    Current := Front;
    Writeln('--- Contenido de la Cola (Mensajes Programados) ---');
    while Current <> nil do
    begin
        Writeln('ID: ', Current^.id);
        Writeln('Remitente: ', Current^.sender);
        Writeln('Destinatario: ', Current^.recipient);
        Writeln('Asunto: ', Current^.subject);
        Writeln('Mensaje: ', Current^.message);
        Writeln('Fecha programada: ', Current^.dateScheduled);
        Writeln('-------------------');
        Current := Current^.Next;
    end;
end;

// ------------------------------
// Ver el nodo en el frente (peek)
// ------------------------------
function Queue_Peek: PMailQueueNode;
begin
    Result := Front;
end;

// ------------------------------
// Contar elementos en la cola
// ------------------------------
function Queue_Count: Integer;
var
    Current: PMailQueueNode;
begin
    Result := 0;
    Current := Front;
    while Current <> nil do
    begin
        Inc(Result);
        Current := Current^.Next;
    end;
end;

// ------------------------------
// Exportar cola como arreglo
// ------------------------------
procedure Queue_ToArray(var Arr: array of PMailQueueNode);
var
    Current: PMailQueueNode;
    i: Integer;
begin
    Current := Front;
    i := 0;
    while (Current <> nil) and (i < Length(Arr)) do
    begin
        Arr[i] := Current;
        Current := Current^.Next;
        Inc(i);
    end;
end;

initialization
    InitQueue; // inicializar punteros al inicio

end.
