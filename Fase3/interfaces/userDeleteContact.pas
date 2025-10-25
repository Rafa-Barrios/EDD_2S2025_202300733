unit userDeleteContact;

{$mode objfpc}{$H+}

interface

procedure ShowUserDeleteContactWindow;

implementation

uses
    gtk2, glib2, SysUtils,
    variables, circularLinkedList,
    jsonTools, userHome;

var
    deleteContactWindow: PGtkWidget;
    lblEmail: PGtkWidget;
    entryEmail: PGtkWidget;
    btnDelete, btnCancel: PGtkWidget;

// -----------------------------
// Función auxiliar para mostrar mensajes
// -----------------------------
procedure ShowInfoMessage(const title, message: string);
var
    dialog: PGtkWidget;
begin
    dialog := gtk_message_dialog_new(
        GTK_WINDOW(deleteContactWindow),
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
    gtk_widget_destroy(deleteContactWindow);
    ShowUserHomeWindow;
end;

// -----------------------------
// Eliminar contacto
// -----------------------------
procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    emailText: string;
    removed: Boolean;
begin
    emailText := gtk_entry_get_text(GTK_ENTRY(entryEmail));

    if Trim(emailText) = '' then
    begin
        ShowInfoMessage('Advertencia', 'Debe ingresar un correo.');
        Exit;
    end;

    // Verificar si el contacto existe en la lista circular en memoria
    removed := CL_RemoveFromList(current_user_contacts, emailText);

    if removed then
    begin
        // Eliminar del JSON también
        if RemoveContactFromJson(json_file_contacts, current_user_username, emailText) then
        begin
            ShowInfoMessage('Éxito', 'Contacto eliminado: ' + emailText);
            gtk_entry_set_text(GTK_ENTRY(entryEmail), '');
        end
        else
        begin
            ShowInfoMessage('Error', 'No se pudo eliminar el contacto del archivo JSON.');
        end;
    end
    else
    begin
        ShowInfoMessage('Error', 'El contacto no existe o no pertenece al usuario actual: ' + emailText);
    end;
end;

// -----------------------------
// Mostrar ventana de eliminar contacto
// -----------------------------
procedure ShowUserDeleteContactWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    deleteContactWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(deleteContactWindow), 'Eliminar Contacto');
    gtk_container_set_border_width(GTK_CONTAINER(deleteContactWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(deleteContactWindow), 400, 150);

    grid := gtk_table_new(2, 2, False);
    gtk_container_add(GTK_CONTAINER(deleteContactWindow), grid);

    lblEmail := gtk_label_new('Correo del contacto:');
    entryEmail := gtk_entry_new;

    gtk_table_attach_defaults(GTK_TABLE(grid), lblEmail, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryEmail, 1, 2, 0, 1);

    btnDelete := gtk_button_new_with_label('Eliminar');
    btnCancel := gtk_button_new_with_label('Cancelar');

    gtk_table_attach_defaults(GTK_TABLE(grid), btnDelete, 1, 2, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 1, 2);

    g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);

    // Al cerrar con la X, usar mismo comportamiento que cancelar
    g_signal_connect(deleteContactWindow, 'destroy', G_CALLBACK(@OnCancelClick), nil);

    gtk_widget_show_all(deleteContactWindow);
    gtk_main;
end;

end.
