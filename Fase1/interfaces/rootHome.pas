unit rootHome;

interface
  procedure ShowRootHomeWindow
;

implementation

    uses
        gtk2, glib2, gdk2, 
        login, rootCreateUser, rootWorkZones,
        interfaceTools, jsonTools, variables, filesTools,
        simpleLinkedList, linkedListOfLists  
    ;


    var
        rootWindow: PGtkWidget;
        btnCreateUser, btnMassiveLoad, btnUserReport, btnRelationReport, btnLogout, btnCreateWorkZone, btnWorkZoneReport: PGtkWidget
    ;


    // Redireccion a crear un usuario
    procedure OnCreateUserClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(rootWindow);
        ShowRootCreateUserWindow;
        end
    ;


    // Carga masiva de usuarios desde JSON
    procedure OnMassiveLoadClick(widget: PGtkWidget; data: gpointer); cdecl;
        var
            status: Boolean;
        begin
        status := jsonTools.UploadUsersFromJson(json_file_path); 
        if status then
            begin
            ShowSuccessMessage(rootWindow, 'Carga de Archivo Json', 'Los usuarios se han cargado correctamente.');
            end
        else
            ShowErrorMessage(rootWindow, 'Carga de Archivo Json', 'Error al cargar usuarios desde JSON.');
        end
    ;


    // Genera los reportes
    procedure OnUserReportClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            filesTools.GenerateReports('users','Root-Reports', LSL_U_GenerateDot());
        end
    ;


    // Genera los reportes de relaciones
    procedure OnRelationReportClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
            LSL_U_PrintToConsole();
        end
    ;


    // Redirecciona al login
    procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(rootWindow);
        ShowLoginWindow;
        end
    ;


    // Redirecciona a crear zonas de trabajo
    procedure OnCreateWorkZoneClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(rootWindow);
        ShowWorkZonesWindow;
        end
    ;


    // Genera reportes de la zona de trabajo
    procedure OnWorkZoneReportClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        filesTools.GenerateReports('workzones','Root-Reports', linkedListOfLists.LL_GenerateDot());
        end
    ;


    // Mostrar la ventana principal rootHome 
    procedure ShowRootHomeWindow;

        var
            grid: PGtkWidget
        ;

        begin
            gtk_init(@argc, @argv);

            // Crear ventana principal
            rootWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(rootWindow), 'Root Home');
            gtk_container_set_border_width(GTK_CONTAINER(rootWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(rootWindow), 300, 400);

            // Crear una tabla para organizar botones (7 filas x 1 columna)
            grid := gtk_table_new(7, 1, False);
            gtk_container_add(GTK_CONTAINER(rootWindow), grid);

            // Crear botones con textos en español
            btnCreateUser := gtk_button_new_with_label('Crear Usuario');
            btnMassiveLoad := gtk_button_new_with_label('Carga Masiva de Usuarios');
            btnUserReport := gtk_button_new_with_label('Reporte de Usuarios');
            btnRelationReport := gtk_button_new_with_label('Reporte de Relaciones');
            btnCreateWorkZone := gtk_button_new_with_label('Zonas de Trabajo');
            btnWorkZoneReport := gtk_button_new_with_label('Reporte Zonas de Trabajo');
            btnLogout := gtk_button_new_with_label('Cerrar Sesion');

            // Conectar señales de click
            g_signal_connect(btnCreateUser, 'clicked', G_CALLBACK(@OnCreateUserClick), nil);
            g_signal_connect(btnMassiveLoad, 'clicked', G_CALLBACK(@OnMassiveLoadClick), nil);
            g_signal_connect(btnUserReport, 'clicked', G_CALLBACK(@OnUserReportClick), nil);
            g_signal_connect(btnRelationReport, 'clicked', G_CALLBACK(@OnRelationReportClick), nil);
            g_signal_connect(btnCreateWorkZone, 'clicked', G_CALLBACK(@OnCreateWorkZoneClick), nil);
            g_signal_connect(btnWorkZoneReport, 'clicked', G_CALLBACK(@OnWorkZoneReportClick), nil);
            g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

            // Ubicar botones en la tabla
            gtk_table_attach_defaults(GTK_TABLE(grid), btnCreateUser,       0, 1, 0, 1);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnMassiveLoad,      0, 1, 1, 2);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnUserReport,       0, 1, 2, 3);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnRelationReport,   0, 1, 3, 4);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnCreateWorkZone,   0, 1, 4, 5);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnWorkZoneReport,   0, 1, 5, 6);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnLogout,           0, 1, 6, 7);

            // Mostrar todos los widgets
            gtk_widget_show_all(rootWindow);

            // Evento para cerrar ventana
            g_signal_connect(rootWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

            // Ejecutar el loop principal de GTK
            gtk_main;
        end
    ;


end.
