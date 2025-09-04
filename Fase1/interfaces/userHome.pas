unit userHome;

interface
    procedure ShowUserHomeWindow;

implementation

    uses
        gtk2, glib2, login, 
        userMailSend,
        userMailProgram,
        userNewContact,
        userViewContact,
        userUpdate,
        variables, filesTools, jsonTools,
        doubleLinkedList;

    var
        userWindow: PGtkWidget;
        lblWelcome: PGtkWidget;
        btnInbox, btnSendMail, btnTrash, btnScheduleMail,
        btnScheduled, btnAddContact, btnContacts, btnUpdateProfile,
        btnReports, btnLogout: PGtkWidget;

    // Bandeja de entrada (antes "Ingresar Vehículo")
    procedure OnInboxClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        writeln('Botón "recibidos" presionado.');
    end;

    // Enviar correo (antes "Insertar vehículo")
    procedure OnSendMailClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(userWindow);
        ShowUserMailSendWindow; // dejamos la funcionalidad existente
    end;

    // Papelera
    procedure OnTrashClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        writeln('Botón "Papelera" presionado.');
    end;

    // Programar correos
    procedure OnScheduleMailClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(userWindow);
        ShowUserMailProgramWindow;
    end;

    // Correos programados
    procedure OnScheduledClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        writeln('Botón "Correos programados" presionado.');
    end;

    // Agregar contactos
    procedure OnAddContactClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(userWindow);
        ShowUserNewContactWindow;
    end;

    // Contactos
    procedure OnContactsClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(userWindow);
        ShowUserViewContactWindow;
    end;

    // Actualizar perfil
    procedure OnUpdateProfileClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        gtk_widget_destroy(userWindow);
        ShowUserUpdateWindow;
    end;

    // Generar reportes (funcionalidad intacta)
    procedure OnReportsClick(widget: PGtkWidget; data: gpointer); cdecl;
    var
        nameFolder : AnsiString;
    begin
        nameFolder := current_user_username + '-Reports';
        filesTools.GenerateReports('vehicules', nameFolder, doubleLinkedList.LDE_V_GenerateDot());
    end;

    // Cerrar sesión (funcionalidad intacta)
    procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
    begin
        doubleLinkedList.LDE_V_Clear();
        gtk_widget_destroy(userWindow);
        ShowLoginWindow;
    end;

    // Mostrar la ventana principal de usuario
    procedure ShowUserHomeWindow;
    var
        grid: PGtkWidget;
        welcomeText: AnsiString; 
    begin
        //Cargar vehículos del usuario
        jsonTools.UploadVehiclesFromJson(json_file_cars, current_user_email);

        gtk_init(@argc, @argv);

        // Crear ventana principal
        userWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(userWindow), 'Inicio Usuario');
        gtk_container_set_border_width(GTK_CONTAINER(userWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(userWindow), 450, 700);

        // Crear una tabla para organizar widgets (10 filas x 1 columna)
        grid := gtk_table_new(10, 1, False);
        gtk_container_add(GTK_CONTAINER(userWindow), grid);

        // Crear etiqueta de bienvenida
        welcomeText := 'Bienvenido: ' + current_user_username;
        lblWelcome := gtk_label_new(PChar(welcomeText));
        gtk_table_attach_defaults(GTK_TABLE(grid), lblWelcome, 0, 1, 0, 1);

        // Crear botones
        btnInbox := gtk_button_new_with_label('Bandeja de entrada');
        btnSendMail := gtk_button_new_with_label('Enviar correo');
        btnTrash := gtk_button_new_with_label('Papelera');
        btnScheduleMail := gtk_button_new_with_label('Programar correos');
        btnScheduled := gtk_button_new_with_label('Correos programados');
        btnAddContact := gtk_button_new_with_label('Agregar contactos');
        btnContacts := gtk_button_new_with_label('Contactos');
        btnUpdateProfile := gtk_button_new_with_label('Actualizar perfil');
        btnReports := gtk_button_new_with_label('Generar reportes');
        btnLogout := gtk_button_new_with_label('Cerrar sesión');

        // Conectar señales de click
        g_signal_connect(btnInbox, 'clicked', G_CALLBACK(@OnInboxClick), nil);
        g_signal_connect(btnSendMail, 'clicked', G_CALLBACK(@OnSendMailClick), nil);
        g_signal_connect(btnTrash, 'clicked', G_CALLBACK(@OnTrashClick), nil);
        g_signal_connect(btnScheduleMail, 'clicked', G_CALLBACK(@OnScheduleMailClick), nil);
        g_signal_connect(btnScheduled, 'clicked', G_CALLBACK(@OnScheduledClick), nil);
        g_signal_connect(btnAddContact, 'clicked', G_CALLBACK(@OnAddContactClick), nil);
        g_signal_connect(btnContacts, 'clicked', G_CALLBACK(@OnContactsClick), nil);
        g_signal_connect(btnUpdateProfile, 'clicked', G_CALLBACK(@OnUpdateProfileClick), nil);
        g_signal_connect(btnReports, 'clicked', G_CALLBACK(@OnReportsClick), nil);
        g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

        // Ubicar botones en la tabla, cada uno en su fila
        gtk_table_attach_defaults(GTK_TABLE(grid), btnInbox,        0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnSendMail,     0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnTrash,        0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnScheduleMail, 0, 1, 4, 5);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnScheduled,    0, 1, 5, 6);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnAddContact,   0, 1, 6, 7);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnContacts,     0, 1, 7, 8);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnUpdateProfile,0, 1, 8, 9);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnReports,      0, 1, 9, 10);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnLogout,       0, 1, 10, 11);

        // Mostrar todos los widgets
        gtk_widget_show_all(userWindow);

        // Evento para cerrar ventana
        g_signal_connect(userWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        // Ejecutar el loop principal de GTK
        gtk_main;
    end;

end.


