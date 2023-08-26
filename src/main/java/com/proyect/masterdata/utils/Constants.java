package com.proyect.masterdata.utils;

public class Constants {
    //Esquemas
    public static final String schemaMaster="master";
    //Tablas
    public static final String tableDepartment= "departamento";
    public static final String tableProvince= "provincia";
    public static final String tableDistrict= "distrito";
    public static final String tableLogin= "sesion";
    public static final String tableUser= "usuario";
    public static final String tableUserType= "tipo_usuario";
    public static final String tableChannel = "canal";
    public static final String tableClient = "cliente";
    public static final String tableMenbresia = "menbresia";
    public static final String tableUserTypeModule = "tipo_usuario_modulo";
    public static final String tablePayment = "pago";
    public static final String tableModule = "modulo";
    public static final String tableDiscount = "descuento";
    public static final String tableEvent = "evento";
    public static final String tablePaymentType = "tipo_pago";
    public static final String tableModuleType = "modulo_tipo";
    public static final String tableConnection = "conexion";

    public static final String tablePaymentMethod = "medios_pago";
    public static final String tablePaymentState = "estados_pago";
    public static final String tableState = "estados";
    public static final String tableSize = "tallas";
    public static final String tableColor = "colores";
    public static final String tableSaleChannel = "canales_venta";
    public static final String tableLogEvent = "eventos_log";
    public static final String tableSizeType = "tipos_talla";
    public static final String tableUserRole = "roles_usuario";
    public static final String tableMembershipType = "tipos_membresia";
    public static final String tableCategory = "categorias";

    //Mensajes
    public static final String register="registration correctly";
    public static final String update="correctly updated";
    public static final String delete="successfully deleted";
    //Errores
    public static final String ErrorWhileRegistering= "Error while registering";
    public static final String ErrorWhileUpdating= "Error while updating";
    public static final String ErrorWhenDeleting= "Error when deleting";

    public static final String ErrorUser= "the user is incorrect";
    public static final String ResultsFound= "no results found";
    public static final String InternalErrorExceptions="Internal Server Error";
    public static final String ErrorDepartment= "The department does not exist";
    public static final String ErrorDepartmentList= "In the list there is a department that exists";
    public static final String ErrorDepartmentExist= "The department does exist";

    public static final String ErrorProvinceList= "In the list there is a province that exists";

    public static final String ErrorProvinceExist= "Exit province";
    public static final String ErrorProvinceNotExist= "Exit The province does not exist";
    public static final String ErrorCategory = "The category does not exist";
    public static final String ErrorCategoryExists = "The category does exist";
    public static final String ErrorCategoryDescriptionExists = "The category description does exist";
    public static final String ErrorCategoryList = "In the list there is a category that exists";
    public static final String ErrorCategoryListDescription = "In the list there is a category with a description that exists";
    public static final String ErrorColor = "The color does not exist";
    public static final String ErrorColorExists = "The color does exist";
    public static final String ErrorColorList = "In the list there is a color thtat exists";
    public static final String ErrorDistrict = "The district does not exist";
    public static final String ErrorPaymentMethod = "The payment method does not exist";
    public static final String ErrorPaymentState = "The payment state does not exist";
    public static final String ErrorSaleChannel = "The sale channel does not exist";
    public static final String ErrorSize = "The size does not exist";
    public static final String ErrorSizeType = "The size type does not exist";
    public static final String ErrorState = "The state does not exist";
    public static final String ErrorUserRole = "The user role does not exist";
}
