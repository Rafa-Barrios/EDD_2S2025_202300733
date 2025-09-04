unit userMailProgram;

interface
    procedure ShowUserMailProgramWindow;

implementation

uses
    gtk2, glib2,
    userHome,
    variables, interfaceTools
;

var
    mailProgramWindow: PGtkWidget;
    lblRecipient, lblSubject, lblMessage, lblDate: PGtkWidget;
    entryRecipient, entrySubject, entryMessage, entryDate: PGtkWidget;
    btnProgram, btnCancel: PGtkWidget;

// Evento Programar correo (funcionalidad pendiente)
procedure OnProgramClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    writeln('Botón "Programar" presionado. Funcionalidad pendiente.');
end;

// Evento Cancelar -> vuelve a la ventana principal
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(mailProgramWindow);
    ShowUserHomeWindow;
end;

// Mostrar ventana de programación de correo
procedure ShowUserMailProgramWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    // Crear ventana principal
    mailProgramWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mailProgramWindow), 'Programar Correo');
    gtk_container_set_border_width(GTK_CONTAINER(mailProgramWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(mailProgramWindow), 450, 450);

    // Crear tabla para organizar etiquetas y entradas (5 filas x 2 columnas)
    grid := gtk_table_new(5, 2, False);
    gtk_container_add(GTK_CONTAINER(mailProgramWindow), grid);

    // Etiqueta y entrada para Destinatario
    lblRecipient := gtk_label_new('Destinatario:');
    entryRecipient := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblRecipient, 0, 1, 0, 1);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryRecipient, 1, 2, 0, 1);

    // Etiqueta y entrada para Asunto
    lblSubject := gtk_label_new('Asunto:');
    entrySubject := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblSubject, 0, 1, 1, 2);
    gtk_table_attach_defaults(GTK_TABLE(grid), entrySubject, 1, 2, 1, 2);

    // Etiqueta y campo para Mensaje (TextView grande)
    lblMessage := gtk_label_new('Mensaje:');
    entryMessage := gtk_text_view_new;
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(entryMessage), GTK_WRAP_WORD);
    gtk_widget_set_size_request(entryMessage, 300, 150);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblMessage, 0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryMessage, 1, 2, 2, 3);

    // Etiqueta y entrada para Fecha
    lblDate := gtk_label_new('Fecha:');
    entryDate := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), lblDate, 0, 1, 3, 4);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryDate, 1, 2, 3, 4);

    // Botones Programar y Cancelar en la última fila
    btnProgram := gtk_button_new_with_label('Programar');
    btnCancel := gtk_button_new_with_label('Cancelar');
    g_signal_connect(btnProgram, 'clicked', G_CALLBACK(@OnProgramClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 4, 5);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnProgram, 1, 2, 4, 5);

    // Mostrar todos los widgets
    gtk_widget_show_all(mailProgramWindow);

    // Evento para cerrar ventana
    g_signal_connect(mailProgramWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_main;
end;

end.

