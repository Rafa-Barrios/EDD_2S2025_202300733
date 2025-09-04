unit userMailSend;

interface
    procedure ShowUserMailSendWindow;

implementation

uses
    gtk2, glib2,
    userHome,
    variables, interfaceTools
;

var
    mailSendWindow: PGtkWidget;
    lblRecipient, lblSubject, lblMessage: PGtkWidget;
    entryRecipient, entrySubject, entryMessage: PGtkWidget;
    btnSend, btnCancel: PGtkWidget;

// Evento Enviar correo (funcionalidad pendiente)
procedure OnSendClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    writeln('Botón "Enviar" presionado. Funcionalidad pendiente.');
end;

// Evento Cancelar -> vuelve a la ventana principal
procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    gtk_widget_destroy(mailSendWindow);
    ShowUserHomeWindow;
end;

// Mostrar ventana de envío de correo
procedure ShowUserMailSendWindow;
var
    grid: PGtkWidget;
begin
    gtk_init(@argc, @argv);

    // Crear ventana principal
    mailSendWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(mailSendWindow), 'Enviar Correo');
    gtk_container_set_border_width(GTK_CONTAINER(mailSendWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(mailSendWindow), 450, 400);

    // Crear tabla para organizar etiquetas y entradas (4 filas x 2 columnas)
    grid := gtk_table_new(4, 2, False);
    gtk_container_add(GTK_CONTAINER(mailSendWindow), grid);

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

    // Etiqueta y campo para Mensaje (TextView más grande)
    lblMessage := gtk_label_new('Mensaje:');
    entryMessage := gtk_text_view_new;
    gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(entryMessage), GTK_WRAP_WORD);
    gtk_widget_set_size_request(entryMessage, 300, 150);
    gtk_table_attach_defaults(GTK_TABLE(grid), lblMessage, 0, 1, 2, 3);
    gtk_table_attach_defaults(GTK_TABLE(grid), entryMessage, 1, 2, 2, 3);

    // Botones Enviar y Cancelar en la última fila
    btnSend := gtk_button_new_with_label('Enviar');
    btnCancel := gtk_button_new_with_label('Cancelar');
    g_signal_connect(btnSend, 'clicked', G_CALLBACK(@OnSendClick), nil);
    g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 3, 4);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnSend, 1, 2, 3, 4);

    // Mostrar todos los widgets
    gtk_widget_show_all(mailSendWindow);

    // Evento para cerrar ventana
    g_signal_connect(mailSendWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    gtk_main;
end;

end.
