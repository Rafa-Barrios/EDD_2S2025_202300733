unit userUpdate;

{$mode objfpc}{$H+}

interface

procedure ShowUserUpdateWindow;

implementation

uses
    gtk2, glib2, SysUtils, variables, simpleLinkedList, jsonTools, userHome;

var
    updateWindow: PGtkWidget;
    lblName, lblEmail, lblUsername, lblPhone: PGtkWidget;
    entryName, entryEmail, entryUsername, entryPhone: PGtkWidget;
    btnUpdate, btnCancel: PGtkWidget;

// Cancelar: cerrar ventana y regresar a home
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(updateWindow);
    ShowUserHomeWindow;
end;

// Actualizar: guardar cambios de usuario y teléfono
procedure OnUpdateClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    usernameText, phoneText: string;
begin
    usernameText := gtk_entry_get_text(GTK_ENTRY(entryUsername));
    phoneText := gtk_entry_get_text(GTK_ENTRY(entryPhone));

    // Actualizar lista enlazada
    LSL_U_Insert(
        current_user_id, // ya es string
        current_user_name,
        current_user_email,
        usernameText,
        phoneText,
        current_user_password
    );

    // Actualizar usuario en JSON sin modificar contraseña
    UpdateUserInJson(
        json_file_path,
        current_user_email,  // buscar por email
        usernameText,        // nuevo username
        phoneText            // nuevo teléfono
    );

    gtk_widget_destroy(updateWindow);
    ShowUserHomeWindow;
end;

// Mostrar ventana de actualización
procedure ShowUserUpdateWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    updateWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(updateWindow), 'Actualizar Usuario');
    gtk_container_set_border_width(GTK_CONTAINER(updateWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(updateWindow), 400, 300);

    grid := gtk_table_new(5, 2, False);
    gtk_container_add(GTK_CONTAINER(updateWindow), grid);

    // Labels
    lblName := gtk_label_new('Nombre:');
    lblEmail := gtk_label_new('Correo:');
    lblUsername := gtk_label_new('Usuario:');
    lblPhone := gtk_label_new('Teléfono:');

    gtk_table_attach_defaults(GTK_TABLE(grid), lblName, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblEmail, 0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblUsername, 0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblPhone, 0, 1, 3, 4);

    // Entrys
    entryName := gtk_entry_new;
    entryEmail := gtk_entry_new;
    entryUsername := gtk_entry_new;
    entryPhone := gtk_entry_new;

    gtk_entry_set_text(GTK_ENTRY(entryName), PChar(AnsiString(current_user_name)));
    gtk_entry_set_text(GTK_ENTRY(entryEmail), PChar(AnsiString(current_user_email)));
    gtk_entry_set_text(GTK_ENTRY(entryUsername), PChar(AnsiString(''))); // vacío por defecto
    gtk_entry_set_text(GTK_ENTRY(entryPhone), PChar(AnsiString('')));    // vacío por defecto

    // Los campos de solo lectura
    gtk_editable_set_editable(GTK_EDITABLE(entryName), False);
    gtk_editable_set_editable(GTK_EDITABLE(entryEmail), False);

    gtk_table_attach_defaults(GTK_TABLE(grid), entryName, 1, 2, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryEmail, 1, 2, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryUsername, 1, 2, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryPhone, 1, 2, 3, 4);

    // Botones
    btnUpdate := gtk_button_new_with_label('Actualizar');
    btnCancel := gtk_button_new_with_label('Cancelar');

    gtk_table_attach_defaults(GTK_TABLE(grid), btnUpdate, 1, 2, 4, 5);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 4, 5);

    g_signal_connect(btnUpdate, 'clicked', G_CALLBACK(@OnUpdateClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
    g_signal_connect(updateWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_widget_show_all(updateWindow);
    gtk_main;
end;

end.