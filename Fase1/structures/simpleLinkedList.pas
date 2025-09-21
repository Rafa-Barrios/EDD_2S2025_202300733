unit simpleLinkedList;

{$MODE DELPHI}

interface

uses
    circularLinkedList; // 🔹 Importamos para usar PCNode

type
    PNode = ^TNode;
    TNode = record
        id: string;
        name: string;
        email: string;
        username: string;
        phone: string;
        password: string;
        Next: PNode;
        contacts: PCNode;   // 🔹 Nueva lista circular de contactos propia de cada usuario
    end;

    TUserData = record
        id: string;
        name: string;
        email: string;
        username: string;
        phone: string;
        password: string;
        // 🔹 Para exportar la lista de contactos si se necesita
        contacts: PCNode;
    end;

procedure LSL_U_Insert(id, name, email, username, phone, password: string);
procedure LSL_U_PrintToConsole;
function  LSL_U_GenerateDot: string;
function  LSL_U_ValidateCredentials(email, password: string): Boolean;
function  LSL_U_GetUserByEmail(const email: string): TUserData;
procedure LSL_U_UpdateUserByEmail(const email, newUsername, newPhone: string);
function  LSL_U_Search(const email: string): PNode; 

// 🔹 NUEVA: Verifica si ya existe un usuario con ese correo
function  LSL_UserExists(const email: string): Boolean;

implementation

uses
    SysUtils, Classes;

var
    Head: PNode = nil;

procedure LSL_U_Insert(id, name, email, username, phone, password: string);
var
    NewNode, Current: PNode;
begin
    New(NewNode);
    NewNode^.id := Trim(id);
    NewNode^.name := Trim(name);
    NewNode^.email := Trim(email);
    NewNode^.username := Trim(username);
    NewNode^.phone := Trim(phone);
    NewNode^.password := Trim(password);
    NewNode^.Next := nil;
    NewNode^.contacts := nil;   // 🔹 Inicializamos la lista circular vacía

    if Head = nil then
        Head := NewNode
    else
    begin
        Current := Head;
        while Current^.Next <> nil do
            Current := Current^.Next;
        Current^.Next := NewNode;
    end;
end;

procedure LSL_U_PrintToConsole;
var
    Current: PNode;
begin
    if Head = nil then
    begin
        Writeln('La lista está vacía.');
        Exit;
    end;

    Current := Head;
    while Current <> nil do
    begin
        Writeln('ID: ', Current^.id);
        Writeln('Nombre: ', Current^.name);
        Writeln('Correo: ', Current^.email);
        Writeln('Usuario: ', Current^.username);
        Writeln('Teléfono: ', Current^.phone);
        Writeln('Password: ', Current^.password);

        if Current^.contacts = nil then
            Writeln('  → Sin contactos')
        else
            Writeln('  → Tiene contactos registrados');

        Writeln('-------------------');
        Current := Current^.Next;
    end;
end;

function LSL_U_GenerateDot: string;
var
    SL: TStringList;
    Current: PNode;
    Counter: Integer;
    NodeName, NextName: string;
    ResultText: string;
begin
    SL := TStringList.Create;

    SL.Add('digraph ListaEnlazada {');
    SL.Add('  rankdir=LR;');
    SL.Add('  nodesep=0.5;');
    SL.Add('');
    SL.Add('  subgraph cluster_0 {');
    SL.Add('    label="Lista simple enlazada";');
    SL.Add('    fontsize=14;');
    SL.Add('    color=black;');
    SL.Add('    style=filled;');
    SL.Add('    fillcolor=white;');
    SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
    SL.Add('');

    if Head = nil then
        SL.Add('    null [label="VACÍA", shape=plaintext];')
    else
    begin
        Counter := 0;
        Current := Head;
        while Current <> nil do
        begin
            NodeName := Format('nodo%d', [Counter]);
            SL.Add(Format('    %s [label="{%s \n %s \n %s \n %s \n %s}"];',
                [NodeName,
                 Current^.id,
                 Current^.name,
                 Current^.username,
                 Current^.email,
                 Current^.phone]));

            if Current^.Next <> nil then
            begin
                NextName := Format('nodo%d', [Counter + 1]);
                SL.Add(Format('    %s -> %s;', [NodeName, NextName]));
            end;

            Inc(Counter);
            Current := Current^.Next;
        end;
    end;

    SL.Add('  }');
    SL.Add('}');

    ResultText := SL.Text;
    SL.Free;

    Result := ResultText;
end;

function LSL_U_ValidateCredentials(email, password: string): Boolean;
var
    Current: PNode;
begin
    Result := False;

    if Head = nil then Exit;

    Current := Head;
    while Current <> nil do
    begin
        if (Current^.email = Trim(email)) and (Current^.password = Trim(password)) then
        begin
            Result := True;
            Exit;
        end;
        Current := Current^.Next;
    end;
end;

function LSL_U_GetUserByEmail(const email: string): TUserData;
var
    Current: PNode;
begin
    Result.id := '';
    Result.name := '';
    Result.email := '';
    Result.username := '';
    Result.phone := '';
    Result.password := '';
    Result.contacts := nil; // 🔹 por defecto

    if Head = nil then Exit;

    Current := Head;
    while Current <> nil do
    begin
        if Current^.email = Trim(email) then
        begin
            Result.id := Current^.id;
            Result.name := Current^.name;
            Result.email := Current^.email;
            Result.username := Current^.username;
            Result.phone := Current^.phone;
            Result.password := Current^.password;
            Result.contacts := Current^.contacts;  // 🔹 exportamos contactos del usuario
            Exit;
        end;
        Current := Current^.Next;
    end;
end;

procedure LSL_U_UpdateUserByEmail(const email, newUsername, newPhone: string);
var
    Current: PNode;
begin
    Current := Head;
    while Current <> nil do
    begin
        if Current^.email = Trim(email) then
        begin
            Current^.username := newUsername;
            Current^.phone := newPhone;
            Exit;
        end;
        Current := Current^.Next;
    end;
end;

function LSL_U_Search(const email: string): PNode;
var
    Current: PNode;
begin
    Result := nil;

    Current := Head;
    while Current <> nil do
    begin
        if Current^.email = Trim(email) then
        begin
            Result := Current;
            Exit;
        end;
        Current := Current^.Next;
    end;
end;

// 🔹 NUEVA FUNCIÓN
function LSL_UserExists(const email: string): Boolean;
var
    Current: PNode;
begin
    Result := False;
    Current := Head;
    while Current <> nil do
    begin
        if Current^.email = Trim(email) then
        begin
            Result := True;
            Exit;
        end;
        Current := Current^.Next;
    end;
end;

end.