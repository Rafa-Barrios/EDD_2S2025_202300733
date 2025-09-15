unit userNewContact;

{$mode objfpc}{$H+}

interface

procedure ShowUserNewContactWindow;

implementation

uses
    gtk2, glib2, SysUtils,
    variables, simpleLinkedList, circularLinkedList,
    jsonTools, userHome;

var
    newContactWindow: PGtkWidget;
    lblEmail: PGtkWidget;
    entryEmail: PGtkWidget;
    btnAdd, btnCancel: PGtkWidget;

// -----------------------------
// Función auxiliar para mostrar mensajes
// -----------------------------
procedure ShowInfoMessage(const title, message: string);
var
    dialog: PGtkWidget;
begin
    dialog := gtk_message_dialog_new(
        GTK_WINDOW(newContactWindow),
        GTK_DIALOG_MODAL,
        GTK_MESSAGE_INFO,
        GTK_BUTTONS_OK,
        PChar(message)
    );
    gtk_window_set_title(GTK_WINDOW(dialog), PChar(title));
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
end;

// -----------------------------
// Cancelar: cerrar ventana y volver a home
// -----------------------------
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(newContactWindow);
    ShowUserHomeWindow; // volvemos a la ventana principal del usuario
end;

// -----------------------------
// Agregar contacto
// -----------------------------
procedure OnAddClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    emailText: string;
    userNode: TUserData;
    success: Boolean;
begin
    emailText := gtk_entry_get_text(GTK_ENTRY(entryEmail));

    if Trim(emailText) = '' then
    begin
        ShowInfoMessage('Advertencia', 'Debe ingresar un correo.');
        Exit;
    end;

    userNode := LSL_U_GetUserByEmail(emailText);

    if userNode.email = '' then
    begin
        ShowInfoMessage('Error', 'Usuario inexistente: ' + emailText);
        Exit;
    end;

    // Guardar contacto en archivo JSON
    success := AddContactToJson(
        json_file_contacts,
        current_user_username,
        userNode.name,
        userNode.username,
        userNode.email,
        userNode.phone
    );

    if success then
    begin
        // También lo agregamos en la lista circular en memoria
        CL_InsertToList(
            current_user_contacts,
            userNode.id,
            userNode.name,
            userNode.email,
            userNode.phone
        );

        ShowInfoMessage('Éxito', 'Contacto agregado: ' + userNode.name + ' (' + userNode.email + ')');
        gtk_entry_set_text(GTK_ENTRY(entryEmail), ''); // limpiar campo
    end
    else
    begin
        ShowInfoMessage('Error', 'No se pudo guardar el contacto en el archivo JSON.');
    end;
end;

// -----------------------------
// Mostrar ventana de nuevo contacto
// -----------------------------
procedure ShowUserNewContactWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    newContactWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(newContactWindow), 'Agregar Contacto');
    gtk_container_set_border_width(GTK_CONTAINER(newContactWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(newContactWindow), 400, 150);

    grid := gtk_table_new(2, 2, False);
    gtk_container_add(GTK_CONTAINER(newContactWindow), grid);

    lblEmail := gtk_label_new('Correo del contacto:');
    entryEmail := gtk_entry_new;

    gtk_table_attach_defaults(GTK_TABLE(grid), lblEmail, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryEmail, 1, 2, 0, 1);

    btnAdd := gtk_button_new_with_label('Agregar');
    btnCancel := gtk_button_new_with_label('Cancelar');

    gtk_table_attach_defaults(GTK_TABLE(grid), btnAdd, 1, 2, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 1, 2);

    g_signal_connect(btnAdd, 'clicked', G_CALLBACK(@OnAddClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);

    // Al cerrar con la X, usar mismo comportamiento que cancelar
    g_signal_connect(newContactWindow, 'destroy', G_CALLBACK(@OnCancelClick), nil);

    gtk_widget_show_all(newContactWindow);
    gtk_main;
end;

end.