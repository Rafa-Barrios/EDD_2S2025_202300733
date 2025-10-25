unit doubleLinkedList;

{$MODE DELPHI}

interface

type
    PMailNode = ^TMailNode;
    TMailNode = record
        id: string;       // ✅ Nuevo campo para identificar el correo
        sender: string;
        subject: string;
        body: string;
        timestamp: string;
        estado: string;   // Estado (ej. leído, no leído, etc.)
        Next: PMailNode;
        Prev: PMailNode;
    end;

procedure DL_InsertToList(id, sender, subject, body, timestamp, estado: string);
procedure DL_ClearList;
function DL_GenerateDot: string;
function DL_GetHead: PMailNode;
function DL_GetTail: PMailNode;

implementation

uses
    SysUtils, Classes;

var
    Head: PMailNode = nil;
    Tail: PMailNode = nil;

// ---------------------------
// Insertar correo al final de la lista doble
// ---------------------------
procedure DL_InsertToList(id, sender, subject, body, timestamp, estado: string);
var
    NewNode: PMailNode;
begin
    New(NewNode);
    NewNode^.id := id;  // ✅ Guardamos el ID
    NewNode^.sender := sender;
    NewNode^.subject := subject;
    NewNode^.body := body;
    NewNode^.timestamp := timestamp;
    NewNode^.estado := estado;
    NewNode^.Next := nil;
    NewNode^.Prev := nil;

    if Head = nil then
    begin
        Head := NewNode;
        Tail := NewNode;
    end
    else
    begin
        Tail^.Next := NewNode;
        NewNode^.Prev := Tail;
        Tail := NewNode;
    end;
end;

// ---------------------------
// Limpiar lista completa
// ---------------------------
procedure DL_ClearList;
var
    Current, Temp: PMailNode;
begin
    Current := Head;
    while Current <> nil do
    begin
        Temp := Current;
        Current := Current^.Next;
        Dispose(Temp);
    end;
    Head := nil;
    Tail := nil;
end;

// ---------------------------
// Obtener primer nodo
// ---------------------------
function DL_GetHead: PMailNode;
begin
    Result := Head;
end;

// ---------------------------
// Obtener último nodo
// ---------------------------
function DL_GetTail: PMailNode;
begin
    Result := Tail;
end;

// ---------------------------
// Escapar caracteres especiales para DOT
// ---------------------------
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

// ---------------------------
// Generar DOT para Graphviz (doble enlace con datos)
// ---------------------------
function DL_GenerateDot: string;
var
    SL: TStringList;
    Current: PMailNode;
    Counter: Integer;
    NodeName, NextName: string;
begin
    SL := TStringList.Create;

    SL.Add('digraph ListaDoble {');
    SL.Add('  rankdir=LR;');
    SL.Add('  nodesep=0.5;');
    SL.Add('  edge [arrowhead=normal];');
    SL.Add('  node [shape=record, style=filled, fillcolor=lightblue];');
    SL.Add('');

    if Head = nil then
    begin
        SL.Add('  null [label="VACÍA", shape=plaintext];');
    end
    else
    begin
        Counter := 0;
        Current := Head;
        while Current <> nil do
        begin
            NodeName := Format('nodo%d', [Counter]);
            SL.Add(Format(
                '  %s [label="{ID: %s\lEstado: %s\lRemitente: %s\lAsunto: %s\lCuerpo: %s\lHora: %s\l}"];',
                [NodeName,
                 EscapeDotString(Current^.id),
                 EscapeDotString(Current^.estado),
                 EscapeDotString(Current^.sender),
                 EscapeDotString(Current^.subject),
                 EscapeDotString(Current^.body),
                 EscapeDotString(Current^.timestamp)]));

            // Conexión al siguiente nodo (doble enlace)
            if Current^.Next <> nil then
            begin
                NextName := Format('nodo%d', [Counter + 1]);
                SL.Add(Format('  %s -> %s;', [NodeName, NextName]));
                SL.Add(Format('  %s -> %s;', [NextName, NodeName]));
            end;

            Inc(Counter);
            Current := Current^.Next;
        end;
    end;

    SL.Add('}');
    Result := SL.Text;
    SL.Free;
end;

end.
