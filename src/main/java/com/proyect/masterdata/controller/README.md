# Controllers documentation

## Overview
API for powip backend

## Endpoints

### POST /auth/login

- Description : log the user in the backend and retrieves a token

- Request : 

{

    "username": username,
    "password": password
}

- Parameters : none

- Response : 

{

    "user": ... user data,
    "jwt": token that authenticates the user
}

- Example :

Url

http://localhost:8080/masterdata/auth/login

Body

{

    "username":"admin1",
    "password":"123abc+"
}

### POST /auth/register

- Description : register new client in the backend service

- Request : 

{
  "username": "rt87",
  "name":"rogelio",
  "surname":"trujillo",
  "email":"rt87@gmail.com",
  "address":"cra 567",
  "mobile":"123456720",
  "dni":"12345678920",
  "category":"moda",
  "users":"11-50",
  "ecommerce":true,
  "billing":false,
  "comment":"administrar pedidos",
  "businessName":"trading company ltda",
  "businessRuc":"12345678920",
  "password": "123abc+",
  "gender":"M",
  "district": "asuncion",
  "store": "e-shop",
  "storeUrl": "e-shop.com",
  "storeType": "shopify",
  "closingChannels" : ["facebook","instagram"],
  "modules": ["Módulo de Gestión","Módulo de Almacén"],
  "entryChannel":"tiktok",
  "demo":true,
  "tokenUser":"REGISTER"
}

- Parameters : none

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

Url

http://localhost:8080/masterdata/auth/register

Body

{
  "username": "rt87",
  "name":"rogelio",
  "surname":"trujillo",
  "email":"rt87@gmail.com",
  "address":"cra 567",
  "mobile":"123456789",
  "dni":"12345678911",
  "category":"tennis",
  "users":10,
  "ecommerce":true,
  "billing":false,
  "comment":"administrar pedidos",
  "businessName":"trading company ltda",
  "businessRuc":"12345678911",
  "password": "123abc+",
  "gender":"M",
  "district": "asuncion",
  "store": "e-shop",
  "storeUrl": "e-shop.com",
  "storeType": "shopify",
  "closingChannels" : ["facebook","instagram"],
  "entryChannel":"tiktok",
  "demo":true,
  "tokenUser":"REGISTER"
}

### GET /brand protected

- Description : list all active brands
- Request: none
- Parameters : 

1. name : filter by name of the brand (not required)
2. tokenUser : filter by user who register or updated the brand
3. sort : set the listing ordering only accepts the values ASC and DESC (not required)
4. sortColumn : set the column which sorts the list (not required)
5. pageNumber : select the page number to view of the list
6. pageSize : set how many records per page have the list

- Response : 

{
    "content": [
        {
            "name": "ADIDAS",
            "client": "SISTEMA",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "NIKE",
            "client": "SISTEMA",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "REBOOK",
            "client": "SISTEMA",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "LEVIS",
            "client": "SISTEMA",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "KENZO",
            "client": "SISTEMA",
            "tokenUser": "ADMIN1"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 5,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 5,
    "size": 5,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 5,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/brand?pageNumber=0&pageSize=3&sort=DESC&sortColumn=name&user=gjimenez

### POST /brand protected

- Description : register one new brand
- Request : none
- Parameters : 

1. name : name of the brand
2. tokenUser : username of the user who registers the brand

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example : 

http://localhost:8080/masterdata/brand?name=nike&tokenUser=admin1

### POST /brand/brands protected

- Description : register two or more new brands
- Request : Array with names of the new brands
- Parameters : 

1. tokenUser : username of the user who registers the brands

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example : 

http://localhost:8080/masterdata/brand/brands?tokenUser=admin1

Body

[
    "reebok","adidas"
]

### GET /cancellation-reason protected

- Description : list all active cancellation reasons
- Request: none
- Parameters : none

- Response :

[
    "NO HAY STOCK",
    "DEMORA EN ENTREGA",
    "MALA CALIDAD",
    "SE LE DAÑO EL PRODUCTO - 30 DIAS",
    "OTROS MOTIVOS",
    "MUY CARO EL ENVIO",
    "ZONA PELIGROSA",
    "CLIENTE NO CONFIABLE PARA CONTRAENTREGA",
    "ROBO POR MOTORIZADO",
    "NO LE GUSTO PRODUCTO"
]

- Example :

http://localhost:8080/masterdata/cancellation-reason

### POST /cancellation-reason protected

- Description : add one cancellation reason to the database
- Request: none
- Parameters :

1. name : name of the cancellation reason
2. tokenUser : username of the user who creates category

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/cancelled-order?tokenUser=admin1

### GET /cancelled-order protected

- Description : list all active cancelled orders

- Request : none

- Parameters :

1. orderId : filter cancelled orders by orderId
2. user : filter suppliers by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "orderId": 2,
            "cancellationReason": "DEMORA EN ENTREGA",
            "registrationDate": "2024-02-02T17:02:46.385+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "paged": true,
        "unpaged": false
    },
    "totalElements": 1,
    "totalPages": 1,
    "last": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "first": true,
    "empty": false
}

- example :

http://localhost:8080/masterdata/cancelled-order?pageNumber=0&pageSize=2&user=gjimenez

### POST /cancelled-order protected

- Description : add one cancelled order to the database and modify the data of the order
- Request: 
{
    orderId:"id number of the order to be cancelled",
    cancellationReason:"reason of the cancellation of the order",
    warehouse:"name of the warehouse to return the stock"
}
- Parameters :

1. tokenUser : username of the user who cancels the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/cancelled-order?tokenUser=admin1

### GET /category

- Description : list all active categories
- Request: none
- Parameters : none

- Response : 

[
    {
        "name": name of the category,
        "description": description of the category
    }
]

- Example :

http://localhost:8080/masterdata/category

### POST /category protected

- Description : add one category to the database
- Request: none
- Parameters : 

1. name : name of category
2. description : description of the category
3. tokenUser : username of the user who creates category

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/category?name=tennis&description=zapatos%20para%20correr&tokenUser=admin1

### POST /category/categories protected

- Description : add multiple categories to the database
- Request :

[

    {
        "name":"name of the category",
        "description:"description of the category"
    },
    ...
]

- Parameters : 

1. user : username of the user who creates categories

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

Url

http://localhost:8080/masterdata/category/categories?tokenUser=admin1

Request :

[
    {

        "name":"camisetas",
        "description":"manga corta sin cuello"
    },
    {

        "name":"blusas",
        "description":"de lana sin mangas"
    }
]

### GET /category-product protected

- Description : list all active category products
- Request: none
- Parameters : 

1. serial : filter category product by serial
2. user : filter category products by the client of the user
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "name": "CAMISETAS",
            "description": "CAMISETAS"
        },
        {
            "name": "JEANS",
            "description": "JEANS"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "paged": true,
        "unpaged": false
    },
    "last": false,
    "totalElements": 5,
    "totalPages": 3,
    "size": 2,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 2,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/category-product

### POST /category-product protected

- Description : add one category product to the database
- Request: none
- Parameters : 

1. name : name of category product
2. description : description of the category product
3. user : username of the user who creates category category product

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/category-product?name=tennis&description=zapatos%20para%20correr&tokenUser=admin1

### POST /category/category-products protected

- Description : add multiple category products to the database
- Request :

[

    {
        "name":"name of the category product",
        "description:"description of the category product"
    },
    ...
]

- Parameters : 

1. user : username of the user who creates categories

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

Url

http://localhost:8080/masterdata/category/category-products?tokenUser=admin1

Response

[
    {

        "name":"camisetas",
        "description":"manga corta sin cuello"
    },
    {

        "name":"blusas",
        "description":"de lana sin mangas"
    }
]

### GET /orderItem protected

- Description : check the stock of one orderItem for the database
- Request: none
- Parameters :

1. productSku : sku of the product 
2. quantity : quantity of the product to check
3. tokenUser : username of the user who checks the product

- Response :

{
    "pendingStock": false,
    "pendingQuantity": 0,
    "itemStockList": [
        {
            "warehouse": "ALCAZAR",
            "stockQuantity": 36
        },
        {
            "warehouse": "ALCAZAR",
            "stockQuantity": 13
        }
    ]
}

- Example :

http://localhost:8080/masterdata/orderItem?productSku=B00002&quantity=5&user=fcasas


### GET /color

- Description : list all the active colors in the database

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": "ROJO"
    },
    {
        "name": "AZUL"
    },
    {
        "name": "VERDE"
    }
]

- Example :

http://localhost:8080/masterdata/color

### POST /color

- Description : add one new color to the database

- Request : none

- Parameters : 

1. name : name of the color
2. tokenUser : username of the user who register the color

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/color?name=rojo&tokenUser=admin1

### POST /color/colors

- Description : add one or more new colors to the database

- Request : Array with names of the colors

- Parameters :

1. tokenUser : username of the user who register the colors

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/color/colors?tokenUser=admin1

Request body

[
    "azul","verde"
]

### GET /courier

- Description : list all the active courier registered in the database

- Request : none

- Parameters :

1. name : filter couriers by name
2. user : filter couriers by client
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "courier": "MARVISUR",
            "phoneNumber": "123456789"
        },
        {
            "courier": "RAPPI",
            "phoneNumber": "111111111"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "totalPages": 1,
    "totalElements": 2,
    "last": true,
    "numberOfElements": 2,
    "size": 2,
    "number": 0,
    "sort": [],
    "first": true,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/courier?pageNumber=0&pageSize=2&user=gjimenez

### POST /courier

- Description : add one new courier to the database

- Request : 
{
 "name":"name of the courier",
 "phoneNumber":"phone number of the courier"
}

- Parameters :

1. tokenUser : username of the user who register the courier

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/courier?tokenUser=admin1

{
    "name":"mavisur",
    "phoneNumber":"123456789"
}

### POST /courier/order

- Description : update the order data

- Request :

{
    "paymentMethod":"name of the paymentMethod",
    "orderState":"name of the order state",
    "orderPictures":"array of files for courier pictures"
}

- Parameters :

1. orderId : id number of the order to update
2. tokenUser : username of the user who register the courier

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/courier?tokenUser=admin1

{
    "paymentMethod":"Link",
    "orderState":"entregado",
    "orderPictures":[...image files]
}

### GET /closing-channel

- Description : list all active closing channels

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the closing channel
    }
]

- Example :

http://localhost:8080/masterdata/closing-channel

### POST /closing-channel protected

- Description : add one closing channel to the database

- Request : none

- Parameters : 

1. name : name of the closing channel
2. tokenUser : username of the user who creates closing channel

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/closing-channel?name=instagram&tokenUser=admin1

### GET /department

- Description : list all active departments

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": "AMAZONAS"
    },
    ... other departments
]

- Example :

http://localhost:8080/masterdata/department

### POST /department protected

- Description : add one department to the database

- Request : none

- Parameters : 

1. name : name of the department
2. user : username of the user who creates department

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/department?name=amazonas&user=admin1


### GET /district/province

- Description : lists all the active districts by province

- Request : none

- Parameters : 

1. province : name of the province

- Response : 

[
    {
        "name": "ASUNCION",
        "nameProvince": "CHACHAPOYAS"
    },
    ... others districts
]

- Example :

http://localhost:8080/masterdata/district/province?province=chachapoyas

### POST /district protected

- Description : add one district to a province in the database

- Request : none

- Parameters : 

1. name : name of the district
2. province : name of the province
3. user : username of the user who creates department

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/district?name=asuncion&province=chachapoyas&user=admin1

### GET /entry-channel

- Description : list all active entry channels

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the entry channel
    }
]

- Example :

http://localhost:8080/masterdata/entry-channel

### POST /entry-channel protected

- Description : add one entry channel to the database

- Request : none

- Parameters : 

1. name : name of the entry channel
2. tokenUser : username of the user who creates entry channel

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/entry-channel?name=facebook&tokenUser=admin1

### GET /general-stock protected

- Description : list general stock

- Request : none

- Parameters : 

1. user : filter warehouses by the client of the user (required)
2. sort : sort the values the only valid values are ASC and DESC, default is ASC
3. sortColumn : select the value that sorts the list in this case name or user
4. pageNumber : the page number to select of the list the first is page zero (0) (required)
5. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "supplierProductSerial": "A00001A",
            "quantity": 15,
            "registrationDate": "2024-01-19T03:53:33.360+00:00",
            "updateDate": "2024-01-19T03:53:33.360+00:00"
        },
        {
            "supplierProductSerial": "A00001B",
            "quantity": 4,
            "registrationDate": "2024-01-19T03:53:33.390+00:00",
            "updateDate": "2024-01-19T03:53:33.390+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "last": false,
    "totalElements": 6,
    "totalPages": 3,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 2,
    "empty": false
}

- example :

http://localhost:8080/masterdata/general-stock?pageNumber=0&pageSize=2&user=gjimenez

### GET /model

- Description : list all the active models registered in the database

- Request : none

- Parameters : 

1. name : filter models by name
2. brand : filter models by brand
3. user : filter models by the username who registered the modules
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "name": "F50",
            "brand": "NIKE",
            "user": "ADMIN1"
        },
        {
            "name": "P90",
            "brand": "NIKE",
            "user": "ADMIN1"
        },
        {
            "name": "MERCURIAL",
            "brand": "NIKE",
            "user": "ADMIN1"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 3,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalElements": 3,
    "totalPages": 1,
    "first": true,
    "size": 15,
    "number": 0,
    "sort": [],
    "numberOfElements": 3,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/model?pageNumber=0&pageSize=3&brand=nike&user=gjimenez

### POST /model protected

- Description : add one new model to the database

- Request : none

- Parameters : 

1. name : name of the model
2. brand : name of the brand of the model
3. tokenUser : username of the user who register the model

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/model?name=f50&brand=nike&tokenUser=admin1

### POST /model/models protected

- Description : add one new models to the database

- Request : Array of models names

- Parameters : 

1. brand : name of the brand of the models
2. tokenUser : username of the user who register the model

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/model?brand=nike&tokenUser=admin1

Request Body

[
    "u pro 3000","k 20000"
]

### GET /module protected

- Description : list all active modules in the database

- Request : none

- Parameters : none

- Response : 

[
    {
        "moduleName": "MÓDULO DE VENTAS",
        "modulePrice": 3.0
    },
    {
        "moduleName": "MÓDULO DE GESTIÓN",
        "modulePrice": 5.0
    },
    {
        "moduleName": "ANALÍTICA DE VENTAS",
        "modulePrice": 3.0
    }
]

- Example :

http://localhost:8080/masterdata/module

### POST /module protected

- Description : add one module to the database

- Request : none

- Parameters : 

1. name : name of the module
2. price : price of the module service by one month
3. tokenUser : username of the user who register the module

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/module?name=ventas&price=2.57&tokenUser=admin1

### GET /onboard protected

- Description : list the onboard records stored in the database

- Request : none

- Parameters : none

- Response : 

[
    {
        "username": "RT87",
        "name": "ROGELIO",
        "surname": "TRUJILLO",
        "email": "rt87@gmail.com",
        "address": "CRA 567",
        "mobile": "123456720",
        "dni": "12345678920",
        "category": "MODA",
        "usersMinimum": 11,
        "usersMaximum": 50,
        "billing": false,
        "comment": "administrar pedidos",
        "businessName": "TRADING COMPANY LTDA",
        "businessRuc": "12345678920",
        "gender": "M",
        "district": "ASUNCION",
        "store": "E-SHOP",
        "storeUrl": "e-shop.com",
        "storeType": "SHOPIFY",
        "closingChannels": [
            "INSTAGRAM",
            "FACEBOOK"
        ],
        "modules": [
            "MÓDULO DE GESTIÓN",
            "MÓDULO DE ALMACÉN"
        ],
        "entryChannel": "TIKTOK",
        "demo": true
    }
    ... more onboarding records
]

- Example :

http://localhost:8080/masterdata/onboard

### POST /order protected

- Description : add one order to the database

- Request : 

{
    "observations":"observations of the seller",
    "receipts":"array with multipart image files of the receipts",
    "deliveryAddress":"address of the delivery for the order",
    "deliveryAmount":"amount of delivery fee with two decimals",
    "advancedPayment":"prepaid amount for the order",
    "saleChannel":"name of the channel of sale",
    "paymentMethod":"name of the payment method",
    "managementType":"name of the management type",
    "requestItems":"array of request items",
    "customerName" : "name of the customer",
    "customerType" : "type of the customer",
    "instagram" : "name of the instagram account",
    "customerPhone": "phone number of the customer",
    "customerAddress" : "address of the customer",
    "customerDistrict" : "district name of the customer", 
    "customerProvince" : "province name of the customer",
    "customerDepartment" : "department of the customer",
    "customerReference" : "reference with additional information of the customer location"
}

- Parameters :

1. tokenUser : username of the user who register the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/order?tokenUser=gjimenez

### PUT /order protected

- Description : update an order of the database

- Request :

{
"orderState":"name of the state of the order",
"observations":"observations about the order",
"paymentMethod":"name of the payment method",
"saleChannel" : "channel of sale",
"receipts":"array of receipts pictures",
"courier":"name of the courier",
"pictures":"array of courier pictures"
}

- Parameters :

1. orderId : id of the order to update
2. tokenUser : username of the user who update the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/order?orderId=1&tokenUser=gjimenez

{
"orderState":"entregado",
"observations":"",
"paymentMethod":"link",
"saleChannel" : "tienda online",
"receipts":[...receipt pictures],
"courier":"marvisur",
"pictures":[...courier pictures]
}

### GET /order protected

- Description : list orders

- Request : none

- Parameters :

1. user : filter orders by the client of the user (required)
2. orderId : filter orders by id
3. orderState : filter orders by order state name
4. courier : filter orders by courier name
5. paymentState : filter orders by payment state name
6. paymentMethod : filter orders by payment method name
7. saleChannel : filter orders by sale channel name
8. managementType : filter orders by management type 
9. sort : sort the values the only valid values are ASC and DESC, default is ASC
10. sortColumn : select the value that sorts the list in this case name or user
11. pageNumber : the page number to select of the list the first is page zero (0) (required)
12. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "id": 1,
            "sellerName": "GONZALO JIMENEZ",
            "customerName": "EMILIO GOMEZ",
            "customerType": "Tradicional",
            "customerPhone": "940544828",
            "instagram": "",
            "department": "LIMA",
            "province": "LIMA",
            "district": "BREÑA",
            "address": "807 IQUIQUE",
            "managementType": "VENTA",
            "paymentMethod": "LINK",
            "saleChannel": "TIENDA ONLINE",
            "reference": "",
            "paymentReceipts": [
                "http://res.cloudinary.com/dqzvbdf9r/image/upload/v1708232900/COMPANY_1_PEDIDOS/PEDIDO_1_GJIMENEZ_2024-02-18_00-08-20_COMPROBANTE_1.jpg"
            ],
            "courierPictures": [
                "http://res.cloudinary.com/dqzvbdf9r/image/upload/v1708232905/COMPANY_1_PEDIDOS/PEDIDO_1_GJIMENEZ_2024-02-18_00-08-24_COURIER_1.jpg"
            ],
            "saleAmount": 10.01,
            "deliveryAmount": 10.01,
            "advancedPayment": 0.0,
            "duePayment": 10.01,
            "registrationDate": "2024-02-18T05:08:20.199+00:00",
            "updateDate": "2024-02-18T05:08:24.818+00:00",
            "deliveryAddress": "807 IQUIQUE",
            "courier": "MARVISUR",
            "items": [
                {
                    "id": 1,
                    "product": {
                        "sku": "A00001",
                        "model": "F90",
                        "size": "12",
                        "category": "TENNIS",
                        "color": "NEGRO",
                        "unit": "PAR",
                        "price": 2.3,
                        "pictures": [
                            "http://res.cloudinary.com/dqzvbdf9r/image/upload/v1708232883/COMPANY_1_PRODUCTOS/PRODUCTO_A00001_GJIMENEZ_2024-02-18_00-08-03_IMAGEN_1.jpg"
                        ]
                    },
                    "quantity": 2,
                    "unitPrice": 2.3,
                    "totalPrice": 4.6,
                    "observations": ""
                },
                {
                    "id": 2,
                    "product": {
                        "sku": "A00002",
                        "model": "M2000",
                        "size": "24",
                        "category": "BOTAS",
                        "color": "ROJO",
                        "unit": "PAR",
                        "price": 5.41,
                        "pictures": [
                            "http://res.cloudinary.com/dqzvbdf9r/image/upload/v1708232884/COMPANY_1_PRODUCTOS/PRODUCTO_A00002_GJIMENEZ_2024-02-18_00-08-04_IMAGEN_1.jpg"
                        ]
                    },
                    "quantity": 1,
                    "unitPrice": 5.41,
                    "totalPrice": 5.41,
                    "observations": ""
                }
            ],
            "orderStatus": "ENTREGADO"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 1,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/order?pageNumber=0&pageSize=2&user=gjimenez&orderId=1&orderState=entregado&courier=marvisur&paymentState=recaudado&paymentMethod=link&saleChannel=tienda online&managementType=venta

### POST /order-item protected

- Description : add one order item to the database

- Request :

{
    "quantity":"quantity of the order item to add to the order",
    "discount":"percentage of the discount to apply in the order item",
    "productSku":"sku of the product to add to the order",
    "observations":"observations about the order item"
}

- Parameters :

1. orderId : id number of the order to add the item
2. tokenUser : username of the user who register the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/order-item?orderId=3&tokenUser=fcasas

{
    "quantity":2,
    "discount":0.00,
    "productSku":"B00002",
    "observations":""
}

### DELETE /order-item protected

- Description : delete one order item of the database

- Request : none

- Parameters :

1. orderId : id number of the order to add the item
2. productSku : product sku of the item to delete from the order
3. tokenUser : username of the user who register the order

- Response :

{

    "code": 200,
    "message": "successfully deleted"
}

- Example :

http://localhost:8080/masterdata/order-item?orderId=4&productSku=B00002&tokenUser=fcasas

### PUT /order-item protected

- Description : update one order item to the database

- Request :

{
    "quantity":"quantity of the order item to update",
    "discount":"percentage of the discount to update in the order item",
    "productSku":"sku of the product to find the order item to update",
    "observations":"update observations about the order item"
}

- Parameters :

1. orderId : id number of the order to add the item
2. tokenUser : username of the user who register the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/order-item?orderId=3&tokenUser=fcasas

{
    "quantity":3,
    "discount":5.00,
    "productSku":"B00001",
    "observations":"adicionan dos unidades del producto en el pedido"
}

### POST /order-stock protected

- Description : add a list of order stocks to the database

- Request :

{
    "quantity":quantity of stock to cover the order orderItem,
    "supplierProductSerial": serial of the supplier product,
    "itemId": id of the orderItem of the order,
    "warehouse" : name of the warehouse 
}

- Parameters :

1. orderId : id of the order that will be stocked
2. tokenUser : username of the user who register the order

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/order-stock?orderId=1&tokenUser=gjimenez

### GET /order-stock protected

- Description : list order stocks

- Request : none

- Parameters :

1. warehouse : filter order stocks by warehouse
2. orderId : filter order stocks by order
3. user : filter orders by the client of the user (required)
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
"content": [
    {
        "orderId": 1,
        "itemId": 1,
        "warehouse": "LUMINOUS",
        "serialSupplierProduct": "A00001A",
        "quantity": 2,
        "registrationDate": "2024-01-29T23:31:25.533+00:00",
        "updateDate": "2024-01-29T23:31:25.533+00:00"
    },
    {
        "orderId": 1,
        "itemId": 2,
        "warehouse": "LUMINOUS",
        "serialSupplierProduct": "A00002A",
        "quantity": 1,
        "registrationDate": "2024-01-29T23:31:25.545+00:00",
        "updateDate": "2024-01-29T23:31:25.545+00:00"
    }
],
"pageable": {
"sort": [],
"offset": 0,
"pageSize": 2,
"pageNumber": 0,
"paged": true,
"unpaged": false
},
"totalPages": 2,
"last": false,
"totalElements": 4,
"size": 2,
"number": 0,
"sort": [],
"first": true,
"numberOfElements": 2,
"empty": false
}

- example :

http://localhost:8080/masterdata/order-stock?pageNumber=0&pageSize=2&user=gjimenez


### GET /product protected

- Description : list all active products

- Request : none

- Parameters : 

1. sku : filter products by sku
2. model : filter products by model
3. user : filter orders by the client of the user (required)
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response : 

{
"content": [
{
"sku": "A00001",
"model": "F90",
"size": "12",
"category": "TENNIS",
"color": "NEGRO",
"unit": "PAR",
"price": 2.3,
"pictures": [
"http://res.cloudinary.com/dqzvbdf9r/image/upload/v1707281503/COMPANY_1_PRODUCTOS/PRODUCTO_A00001_GJIMENEZ_2024-02-06_23-51-43_IMAGEN_1.jpg"
]
},
{
"sku": "A00002",
"model": "M2000",
"size": "24",
"category": "BOTAS",
"color": "ROJO",
"unit": "PAR",
"price": 5.41,
"pictures": [
"http://res.cloudinary.com/dqzvbdf9r/image/upload/v1707281504/COMPANY_1_PRODUCTOS/PRODUCTO_A00002_GJIMENEZ_2024-02-06_23-51-44_IMAGEN_1.jpg"
]
}
],
"pageable": {
"sort": [],
"offset": 0,
"pageSize": 2,
"pageNumber": 0,
"paged": true,
"unpaged": false
},
"totalPages": 5,
"totalElements": 9,
"last": false,
"size": 2,
"number": 0,
"sort": [],
"numberOfElements": 2,
"first": true,
"empty": false
}}

- Example :

http://localhost:8080/masterdata/product?pageNumber=0&pageSize=2&user=gjimenez


### POST /product

- Description : register one product in the database

- Request : 

{
    sku : sku serial,
    model : name of model which the product belongs,
    color : name of the color of the product,
    category : name of the category of the product,
    size : name of the size of the product,
    unit : name of the unit of the product,
    pictures : multipart image files of the product in an array
}

- Params : 

1. tokenUser : user who registers the product in the database

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example

http://localhost:8080/masterdata/product?tokenUser=admin1

Request body

{
    "sku":"Y2K9",
    "model":"F50",
    "color":"rojo",
    "size":"M",
    "category":"tennis",
    "unit":"par",
    "pictures":[image files...] 
}

### POST /product/products

- Description : register two or more products in the database

- Request : 

[
    {
        sku : sku serial,
        model : name of model which the product belongs,
        color : name of the color of the product,
        category : name of the category of the product,
        size : name of the size of the product,
        unit : name of the unit of the product
    },
    ... more products
]

- Params : 

1. tokenUser : user who registers the products in the database

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example

http://localhost:8080/masterdata/product?tokenUser=admin1

Request body

[
    {
        "sku":"Y2K9",
        "model":"F50",
        "color":"rojo",
        "size":"36"
    },
    {
        "sku":"E4T6",
        "model":"K 20000",
        "color":"verde",
        "size":"32"
    }
]

### GET /province/department

- Description : list all active provinces by department

- Request : none

- Parameters : 

1. department : name of the department 

- Response : 

[
    {
        "name": "CHACHAPOYAS",
        "nameDepartment": "AMAZONAS"
    },
    ... others provinces
]

- Example :

http://localhost:8080/masterdata/province/department?department=amazonas

### POST /province protected

- Description : add one province to the department

- Request : none

- Parameters : 

1. name : name of the province
2. department : name of the department 
3. user : username of the user who creates province

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/province?name=chachapoyas&department=amazonas&user=admin1

### GET /purchase protected

- Description : list all active purchases

- Request : none

- Parameters : 

1. serial : filter purchases by serial
2. user : filter purchases by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "serial": "AA00001",
            "registrationDate": "2024-02-14T04:13:38.623+00:00"
        }
    ],
    "pageable": {
    "sort": [],
    "offset": 0,
    "pageNumber": 0,
    "pageSize": 2,
    "paged": true,
    "unpaged": false
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- Example : 

http://localhost:8080/masterdata/purchase?user=gjimenez&pageNumber=0&pageSize=2&serial=AA00001

### GET /purchase-orderItem protected

- Description : list all active purchase items

- Request : none

- Parameters :

1. serial : filter purchase items by serial
2. user : filter purchase items by the client of the user (required)
3. supplierProductSerial : filter purchase items by supplier product serial
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "serial": "AA00001",
            "quantity": 15,
            "supplierProductSerial": "A00001A",
            "unitPrice": 3.4,
            "date": "2024-02-14T04:51:51.638+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "totalElements": 1,
    "totalPages": 1,
    "last": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "first": true,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/purchase-orderItem?user=gjimenez&pageNumber=0&pageSize=2


### POST /purchaseItem protected

- Description : add one purchaseItem to the database

- Request : 

[
    {
        "quantity": number of purchased units,
        "supplierProductSerial": serial of the provider product,
        "unitPrice": unit price of the product
    }
]

- Parameters : 

1. serial : serial of the purchaseItem ordering
2. tokenUser : username of the user who creates the purchaseItem

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/purchase-orderItem?serial=AA00001&tokenUser=gjimenez

[
    {
        "quantity": 15,
        "supplierProductSerial": "A00001A",
        "unitPrice": 4.70
    }
    ... more items
]

### GET /shipment-orderItem protected

- Description : list all shipment items

- Request : none

- Parameters : 

1. purchaseSerial : filter shipment items by purchase serial
2. user : filter shipment items by the client of the user (required)
3. supplierProductSerial : filter shipment items by supplier product serial
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "serial": "AA00001",
            "warehouse": "LUMINOUS",
            "quantity": 15,
            "supplierProductSerial": "A00001A",
            "purchaseSerial": "AA00001",
            "date": "2024-02-14T02:45:20.582+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 1,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- Example : 

http://localhost:8080/masterdata/shipment-orderItem?user=gjimenez&pageNumber=0&pageSize=2&purchaseSerial=AA00001&supplierProductSerial=A00001A

### GET /shipment protected

- Description : list all active shipments

- Request : none

- Parameters :

1. purchaseSerial : filter shipments by purchase serial
2. warehouse : filter shipments by warehouse
3. user : filter shipments by the client of the user (required)
4. shipmentType : filter shipments by shipment type
5. sort : sort the values the only valid values are ASC and DESC, default is ASC
6. sortColumn : select the value that sorts the list in this case name or user
7. pageNumber : the page number to select of the list the first is page zero (0) (required)
8. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "purchaseSerial": "AA00001",
            "warehouse": "LUMINOUS",
            "shipmentType": "EMBARQUE",
            "registrationDate": "2024-02-14T01:12:22.576+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 1,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/shipment?user=gjimenez&pageNumber=0&pageSize=2&warehouse=luminous&shipmentType=embarque

### POST /shipment protected

- Description : add one shipment to the database

- Request : 

{
    "purchaseSerial":"serial of the purchase of the shipment goods",
    "warehouse":"name of the shipment warehouse",
    "shipmentType":"name of the shipment type",
    "requestShipmentList": [
        {
            "quantity": number of units,
            "observations": observation of the state of the goods,
            "supplierProductSerial": serial of the provider product,
            "purchaseSerial": serial of the purchaseItem ordering of the goods
        }
        ... more items
    ]
}

- Parameters : 

1. tokenUser : username of who register the shipment

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/shipment-orderItem?serial=SA00001&warehouse=luminous&tokenUser=gjimenez

[
    {
        "quantity": 15,
        "observations":"en buen estado",
        "supplierProductSerial": "A00001A",
        "purchaseSerial": "AA00001"
    }
    ... more items
]

### GET /shipment-type protected

- Description : list active shipment types

- Request : none

- Parameters : none

- Response :

[
    "name of the shipment type"
]

- Example :

http://localhost:8080/masterdata/shipment-type

### POST /shipment-type protected

- Description : add one shipment type to the database

- Request : none

- Parameters :

1. name : name of the shipment type
2. tokenUser : username of the user who creates the shipment type

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/shipment-type?name=embarque&tokenUser=admin1

### GET /size-type protected

- Description : list all active size types

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the size type
    }
]

- Example :

http://localhost:8080/masterdata/size-type

### POST /size-type protected

- Description : add one size type to the database

- Request : none

- Parameters : 

1. name : name of the size type
2. tokenUser : username of the user who creates size type 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/size-type?name=ropa&tokenUser=admin1

### POST /size-type/size-types protected

- Description : add one or more size types to the database

- Request : none

- Parameters : 

1. tokenUser : username of the user who creates size type 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/size-type?tokenUser=admin1

Request Body

[
    "celulares"
]

### GET /size protected

- Description : list all active sizes

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the size,
        "sizeType": name of the size Type
    },
    ... more sizes
]

- Example :

http://localhost:8080/masterdata/size

### POST /size protected

- Description : add one size to the database

- Request : none

- Parameters : 

1. name : name of the size
2. sizeType : name of the size type of the size
2. tokenUser : username of the user who creates size type 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/size?name=s&sizeType=ropa&tokenUser=admin1

### POST /size/sizes protected

- Description : add one or more sizes to the database

- Request : none

- Parameters : 

1. name : name of the size
2. sizeType : name of the size type of the size
3. tokenUser : username of the user who creates size 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/size/sizes?sizeType=ropa&tokenUser=admin1

Request Body

[
    "m","l","xs","xl"
]

### GET /stock-replenishment

- Description : list active stock replenishments

- Request : none

- Parameter :

1. user : name of the user who is listing the stock return (required)
2. orderId : order id number for the order that request stock replenishment
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "orderId": 5,
            "registrationDate": "2024-02-16T01:07:05.850+00:00",
            "updateDate": "2024-02-16T01:07:05.850+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 4,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-replenishment?user=gjimenez&pageNumber=0&pageSize=4&orderId=5

### POST /stock-replenishment protected

- Description : add one stock replenishment to the database

    - Request :
      [
          {
            "productSku" : "sku of the product to replenish",
            "quantity" : "quantity of the product to replenish"
          }
      ]

- Parameters :

1. orderId : id of the order to replenish the stock
2. tokenUser : username of the user who register the stock replenish

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/stock-replenishment?orderId=5&tokenUser=gjimenez

[
    {
        "productSku" : "A00001",
        "quantity" : 5
    }
]

### GET /stock-replenishment-item

- Description : list active stock replenishment items

- Request : none

- Parameter :

1. user : name of the user who is listing the stock return (required)
2. orderId : order id number for the order that request stock replenishment
3. productSku : product sku code of the product belonging to the stock replenishment
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "orderId": 5,
            "productSku": "A00003",
            "quantity": 5,
            "registrationDate": "2024-02-16T03:18:34.276+00:00",
            "updateDate": "2024-02-16T03:18:34.276+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 4,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-replenishment-item?user=gjimenez&pageNumber=0&pageSize=4&orderId=5&productSku=A00003

### GET /stock-return

- Description : list active stock return items per client

- Request : none

- Parameter :

1. user : name of the user who is listing the stock return (required)
2. purchaseSerial : serial purchaseItem of the return stock
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "purchaseSerial": "AA00001",
            "registrationDate": "2024-02-15T02:53:16.034+00:00",
            "updateDate": "2024-02-15T02:53:16.034+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 4,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-return?user=gjimenez&pageNumber=0&pageSize=4&purchaseSerial=AA00001

### POST /stock-return protected

- Description : add one stock return to the database

  - Request : 
  [
      {
        "supplierProductSerial" : "serial of the stocked product from supplier",
        "quantity" : "quantity of the returned units",
        "observations" : "observations about the units to return"
      }
  ]

- Parameters :

1. purchaseSerial : serial of the purchaseItem of the returned stock
2. tokenUser : username of the user who register the return

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/stock-return?purchaseSerial=AA00001&tokenUser=gjimenez

[
    {
        "supplierProductSerial" : "A00001A",
        "quantity" : 4,
        "observations" : "unidades dañadas por agua"
    }
]

### GET /stock-return-item

- Description : list active stock return items per client

- Request : none

- Parameter :

1. user : name of the user who is listing the stock return (required)
2. purchaseSerial : serial purchaseItem of the return stock
3. supplierProductSerial : serial of the supplier product
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "purchaseSerial": "AA00001",
            "supplierProductSerial": "A00001A",
            "quantity": 4,
            "observations": "unidades dañadas por agua",
            "registrationDate": "2024-02-15T02:53:16.044+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 4,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-return-orderItem?user=gjimenez&pageNumber=0&pageSize=4&purchaseSerial=AA00001&supplierProductSerial=A00001A

### GET /stock-transaction

- Description : list stock transactions per client

- Request : none

- Parameters :

1. user : name of the user who is listing the stock transactions (required)
2. serial : serial of the stock transaction
3. warehouse : name of the warehouse involved in the transactions
4. stockTransactionType : stock transaction type of the stock transaction
5. sort : sort the values the only valid values are ASC and DESC, default is ASC
6. sortColumn : select the value that sorts the list in this case name or user
7. pageNumber : the page number to select of the list the first is page zero (0) (required)
8. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "serial": "SAA00001",
            "warehouse": "LUMINOUS",
            "stockTransactionType": "ENTRADA",
            "registrationDate": "2024-02-14T08:15:10.100+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 4,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-transaction?user=gjimenez&pageNumber=0&pageSize=4&warehouse=luminous&serial=SAA00001&stockTransactionType=entrada

### GET /stock-transaction-item

- Description : list stock transaction items

- Request : none

- Parameters :

1. user : filter the stock transaction items by client (required)
2. stockTransactionSerial : filter stock transaction items by stock transaction serial 
3. supplierProductSerial : filter stock transaction items by supplier product serial
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "quantity": 15,
            "warehouse": "LUMINOUS",
            "stockTransactionSerial": "SAA00001",
            "stockTransactionType": "ENTRADA",
            "supplierProductSerial": "A00001A",
            "date": "2024-02-14T17:21:50.367+00:00"
        }
    ],
    "pageable": {
    "sort": [],
    "offset": 0,
    "pageSize": 4,
    "pageNumber": 0,
    "unpaged": false,
    "paged": true
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-transaction-orderItem?user=gjimenez&pageNumber=0&pageSize=4&stockTransactionSerial=SAA00001&supplierProductSerial=A00001A

### GET /stock-transaction-type

- Description : list active stock transactions types

- Request : none

- Parameters : none

- Response :

[
    {
        "name": "ENTRADA"
    },
    {
        "name": "SALIDA"
    },
    {
        "name": "TRANSFERENCIA"
    },
    {
        "name": "DEVOLUCION-COMPRADOR"
    },
    {
        "name": "DEVOLUCION-PROVEEDOR"
    }
]

- example :

http://localhost:8080/masterdata/stock-transaction-type

### POST /stock-transaction-type protected

- Description : add one store type to the database

- Request : none

- Parameters :

1. name : name of the stock transaction type
2. tokenUser : username of the user who creates stock transaction type

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/stock-transaction-type?name=entrada&tokenUser=admin1

### GET /stock-transfer

- Description : list stock transfers

- Request : none

- Parameters :

1. user : filter the stock transfers by client (required)
2. originWarehouse : filter stock transfers by origin warehouse name
3. destinationWarehouse : filter stock transfer by supplier destination warehouse name
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "stockTransferId": 1,
            "originWarehouse": "LUMINOUS",
            "destinationWarehouse": "OIKAS",
            "registrationDate": "2024-02-17T03:40:39.346+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 4,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-transfer?user=gjimenez&pageNumber=0&pageSize=4&originWarehouse=luminous&destinationWarehouse=oikas

### POST /stock-transfer protected

- Description : add one stock transfer to the database

- Request :

  - Request Stock Transfer : 

    {
        "originWarehouse":"name of the origin warehouse",
        "destinationWarehouse" : "name of the destination warehouse",
        "requestStockTransferItemsList" : [
            {
                "supplierProductSerial" : "serial of the supplier product to be transfer",
                "quantity" : "quantity of the supplier product to be transfer"
            }
        ] 
    }

- Parameters :

1. purchaseSerial : serial of the purchaseItem of the returned stock
2. tokenUser : username of the user who register the return

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/stock-transfer?tokenUser=gjimenez

{
    "originWarehouse" : "",
    "destinationWarehouse" : ""
}

[
    {
        "supplierProductSerial" : "A00001A",
        "quantity" : 4
    }
]

### GET /stock-transfer-item

- Description : list stock transfer items

- Request : none

- Parameters :

1. user : filter the stock transfer items by client (required)
2. stockTransferId : filter stock transfer items by stock transfer id
3. supplierProductSerial : filter stock transfer items by supplier product serial
4. sort : sort the values the only valid values are ASC and DESC, default is ASC
5. sortColumn : select the value that sorts the list in this case name or user
6. pageNumber : the page number to select of the list the first is page zero (0) (required)
7. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "stockTransferId": 1,
            "quantity": 15,
            "supplierProductSerial": "A00003B",
            "registrationDate": "2024-02-17T05:35:38.265+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 4,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "first": true,
    "size": 4,
    "number": 0,
    "sort": [],
    "numberOfElements": 1,
    "empty": false
}

- example :

http://localhost:8080/masterdata/stock-transfer-item?user=gjimenez&pageNumber=0&pageSize=4&stockTransferId=1&supplierProductSerial=A00003B

### GET /store-type

- Description : list all active store types

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the store type
    }
]

- Example :

http://localhost:8080/masterdata/store-type

### POST /store-type protected

- Description : add one store type to the database

- Request : none

- Parameters : 

1. name : name of the store type
2. tokenUser : username of the user who creates store type 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/store-type?name=shopify&tokenUser=admin1

### GET /subscription

- Description : list all the active subscriptions registered in the database

- Request : none

- Parameters : 

1. name : filter subscriptions by name
2. user : filter subscriptions by the username who registered them
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "name": "SEMESTRAL",
            "months": 6,
            "discountPercent": 5.0
        },
        {
            "name": "ANUAL",
            "months": 12,
            "discountPercent": 10.0
        },
        {
            "name": "MENSUAL",
            "months": 1,
            "discountPercent": 0.0
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 5,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 3,
    "first": true,
    "size": 5,
    "number": 0,
    "sort": [],
    "numberOfElements": 3,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/subscription?pageNumber=0&pageSize=5&sort=ASC&sortColumn=name

### POST /subscription protected

- Description : add one subscription to the database

- Request : none

- Parameters : 

1. name : name of the subscription
2. months : months of duration for the subscription
3. price : price of the subscription for one month
4. tokenUser : username of the user who creates the subscription 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/subscription?name=semestral&months=6&discountPercent=5.00&tokenUser=admin1

### GET /subscription/plans

- Description : list the subscriptions and all the active modules with the discounted price

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": "SEMESTRAL",
        "months": 6,
        "discountPercentaje": 5.0,
        "moduleList": [
            {
                "moduleName": "VENTAS",
                "modulePrice": 14.65
            },
            {
                "moduleName": "FINANZAS",
                "modulePrice": 28.56
            },
            {
                "moduleName": "INVENTARIO",
                "modulePrice": 22.74
            },
            {
                "moduleName": "MARKETING",
                "modulePrice": 57.17
            },
            {
                "moduleName": "COURIER",
                "modulePrice": 52.50
            }
        ]
    },
    {
        "name": "ANUAL",
        "months": 12,
        "discountPercentaje": 10.0,
        "moduleList": [
            {
                "moduleName": "VENTAS",
                "modulePrice": 27.76
            },
            {
                "moduleName": "FINANZAS",
                "modulePrice": 54.11
            },
            {
                "moduleName": "INVENTARIO",
                "modulePrice": 43.09
            },
            {
                "moduleName": "MARKETING",
                "modulePrice": 108.32
            },
            {
                "moduleName": "COURIER",
                "modulePrice": 99.47
            }
        ]
    },
    {
        "name": "MENSUAL",
        "months": 1,
        "discountPercentaje": 0.0,
        "moduleList": [
            {
                "moduleName": "VENTAS",
                "modulePrice": 2.57
            },
            {
                "moduleName": "FINANZAS",
                "modulePrice": 5.01
            },
            {
                "moduleName": "INVENTARIO",
                "modulePrice": 3.99
            },
            {
                "moduleName": "MARKETING",
                "modulePrice": 10.03
            },
            {
                "moduleName": "COURIER",
                "modulePrice": 9.21
            }
        ]
    }
]

- Example :

http://localhost:8080/masterdata/subscription/plans

### GET /supplier protected

- Description : list all active suppliers

- Request : none

- Parameters : 

1. name : filter suppliers by name
2. user : filter suppliers by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "businessName": "BURGENVILLIA .CORP",
            "ruc": "12345678922",
            "country": "PERU",
            "location": "LIMA, STREET 123",
            "phoneNumber": "323456789",
            "email": "bg@gmail.com"
        },
        {
            "businessName": "COLTRAN LTD",
            "ruc": "12345678924",
            "country": "INDIA",
            "location": "MUMBAI, AV 345",
            "phoneNumber": "333456789",
            "email": "coltran@gmail.com"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "paged": true,
        "unpaged": false
    },
    "totalPages": 1,
    "totalElements": 2,
    "last": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 2,
    "first": true,
    "empty": false
}

- example :

http://localhost:8080/masterdata/supplier?pageNumber=0&pageSize=2&user=gjimenez

### POST /supplier protected

- Description : add one supplier to the database

- Request : 

{
    "businessName": supplier business name,
    "ruc": business id number,
    "country": country of the supplier,
    "location": location of the supplier,
    "phoneNumber": phone number of the supplier,
    "email": email of the supplier
}

- Parameters : 

1. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/supplier?tokenUser=gjimenez

{
    "businessName": "BURGENVILLIA .CORP",
    "ruc": "12345678922",
    "country": "PERU",
    "location": "LIMA, STREET 123",
    "phoneNumber": "323456789",
    "email": "bg@gmail.com"
}

### POST /supplier/suppliers protected

- Description : add one or more supplier to the database

- Request : 

[
    {
        "businessName": supplier business name,
        "ruc": business id number,
        "country": country of the supplier,
        "location": location of the supplier,
        "phoneNumber": phone number of the supplier,
        "email": email of the supplier
    },
    ... more suppliers
]

- Parameters : 

1. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/supplier?tokenUser=gjimenez

[
    {
        "businessName": "BURGENVILLIA .CORP",
        "ruc": "12345678922",
        "country": "PERU",
        "location": "LIMA, STREET 123",
        "phoneNumber": "323456789",
        "email": "bg@gmail.com"
    },
    {
        "businessName": "COLTRAN LTD",
        "ruc": "12345678924",
        "country": "INDIA",
        "location": "MUMBAI, AV 345",
        "phoneNumber": "333456789",
        "email": "coltran@gmail.com"
    }
]

### GET /supplier-product protected

- Description : list all active supplier products

- Request : none

- Parameters : 

1. serial : filter supplier product by serial
2. user : filter supplier products by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "serial": "A00001A",
            "productSku": "A00001",
            "supplierRuc": "BURGENVILLIA .CORP",
            "purchasePrice": 5.24
        },
        {
            "serial": "A00001B",
            "productSku": "A00001",
            "supplierRuc": "COLTRAN LTD",
            "purchasePrice": 2.1
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": false,
    "totalElements": 36,
    "totalPages": 18,
    "size": 2,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 2,
    "empty": false
}

- Example : 

http://localhost:8080/masterdata/supplier-product?user=gjimenez&pageNumber=0&pageSize=2

### POST /supplier-product protected

- Description : add one supplier product to the database

- Request : 

{
    "serial": serial number of the supplier product,
    "productSku": sku of the product,
    "supplierRuc": ruc of the supplier,
    "purchasePrice": price of purchaseItem per unit
}

- Parameters : 

1. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/supplier-product?tokenUser=gjimenez

{
    "serial":"A00001A",
    "productSku": "A00001",
    "supplierRuc": "12345678922",
    "purchasePrice": 5.24
}

### POST /supplier-product/supplier-products protected

- Description : add one or more supplier product to the database

- Request : 

[
    {
        "serial": serial number of the supplier product,
        "productSku": sku of the product,
        "supplierRuc": ruc of the supplier,
        "purchasePrice": price of purchaseItem per unit
    },
    ... more suppliers
]

- Parameters : 

1. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/supplier-product/supplier-products?tokenUser=gjimenez

[
    {
        "serial":"A00001A",
        "productSku": "A00001",
        "supplierRuc": "12345678922",
        "purchasePrice": 5.24
    },
    ...
]

### GET /stock-transaction-type protected

- Description : list all active stock transaction types

- Request : none

- Parameters : none

- Response : 

{
    "content": [
        {
            "name":"entrada"
        },
        {
            "name":"salida"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": false,
    "totalElements": 2,
    "totalPages": 1,
    "size": 2,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 2,
    "empty": false
}

- Example : 

http://localhost:8080/masterdata/stock-transaction-type

### POST /stock-transaction-type protected

- Description : add one stock transaction type to the database

- Request : none

- Parameters : 

1. name : name of the new stock transaction type
2. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/stock-transaction-type?name=entrada&tokenUser=admin1

### POST /stock-transaction-type/stock-transaction-types protected

- Description : add one or more stock transaction types to the database

- Request : array with names of the stock transaction types.

- Parameters : 

1. tokenUser : username of the user who creates the supplier

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/supplier?tokenUser=admin1

[
    "entrada","salida"
]


### GET /user protected

- Description : list all active users

- Request : none

- Parameters : 

1. user : filter supplier product by serial
2. clientId : filter supplier products by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "dni": "12345678910",
            "user": "GJIMENEZ",
            "name": "GONZALO",
            "surname": "JIMENEZ",
            "email": "gj@gmail.com",
            "district": "SISTEMA",
            "address": "CRA 123",
            "mobile": "123456789",
            "gender": "M",
            "status": null
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 2,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalElements": 1,
    "totalPages": 1,
    "size": 2,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 1,
    "empty": false
}

- Example : 

http://localhost:8080/masterdata/user?pageNumber=0&pageSize=2&clientRuc=12345678910

### POST /user protected

- Description : add one user to the database

- Request : 

{
    "user":"username of the user",
    "name":"name of the user",
    "surname":"surname of the user",
    "dni":"dni of the user",
    "email":"email address of the user",
    "address":"phisical adress of the user",
    "gender" : "gender of the user",
    "mobile" : "mobile number",
    "password" : "password of the user",
    "district" : "origin district of the user",
    "clientRuc" : "ruc of the client who belongs the user",
    "tokenUser" : "username of who registered the user"
}

- Parameters : none

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/user

{
    "user":"gjimenez",
    "name":"gonzalo",
    "surname":"jimenez",
    "dni":"12345678910",
    "email":"gj@gmail.com",
    "address":"cra 123",
    "gender" : "M",
    "mobile" : "123456789",
    "password" : "123abc+",
    "district" : "sistema",
    "clientRuc" : "12345678910",
    "tokenUser" : "admin1"
}

### GET /unit protected

- Description : list all active units

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the unit,
        "unitType": name of the unitType
    }
    ... more units
]

- Example :

http://localhost:8080/masterdata/unit

### POST /unit protected

- Description : add one unit to the database

- Request : 

{
    "name":name of the unit,
    "unitType":name of the unit type
}

- Parameters : 

1. tokenUser : username of the user who creates unit

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit?tokenUser=admin1

### POST /unit/units protected

- Description : add one or more units to the database

- Request : array with the name of the new units

- Parameters : 

1. unit type : name of the unit type of the units
2. tokenUser : username of the user who creates the units

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit/units?tokenUser=admin1

### GET /unit-type protected

- Description : list all active unit types

- Request : none

- Parameters : none

- Response :

[
{
"name": name of the unit type
}
... more unit types
]

- Example :

http://localhost:8080/masterdata/unit-type

### POST /unit-type protected

- Description : add one unit type to the database

- Request : none

- Parameters :

1. name : name of the unit type
2. tokenUser : username of the user who creates unit type

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit-type?name=ropa&tokenUser=admin1

### POST /unit/units protected

- Description : add one or more unit types to the database

- Request : array with the name of the new unit types

- Parameters :

1. tokenUser : username of the user who creates the unit type

- Response :

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit-type/unit-types?tokenUser=admin1

### GET /warehouse protected

- Description : list all active warehouses

- Request : none

- Parameters : 

1. name : filter warehouses by name
2. user : filter warehouses by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "name": "LUMINOUS",
            "location": "CUSCO CALLE 123"
        },
        {
            "name": "OIKAS",
            "location": "LIMA AVENIDA 234"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "unpaged": false,
        "paged": true
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 2,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 2,
    "empty": false
}

- example :

http://localhost:8080/masterdata/warehouse?pageNumber=0&pageSize=1&user=gjimenez

### POST /warehouse protected

- Description : add one warehouse to the database

- Request : 
{
    "name":"name of the warehouse",
    "location":"location of the warehouse"
}

- Parameters : 

1. tokenUser : username of the user who creates warehouse

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/warehouse?tokenUser=gjimenez

{
    "name":"luminous",
    "location":"Cusco calle 123"
}

### POST /warehouse/warehouses protected

- Description : add one or more warehouses to the database

- Request : 
[
    {
        "name":"name of the warehouse",
        "location":"location of the warehouse"
    }
]

- Parameters : 

1. tokenUser : username of the user who creates the warehouses

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/warehouse/warehouses?tokenUser=gjimenez

[
    {
        "name":"luminous",
        "location":"Cusco calle 123"
    }
]

### GET /warehouse-stock protected

- Description : list warehouse stock from an warehouse

- Request : none

- Parameters : 

1. warehouse : filter warehouse stock by warehouse (required)
2. user : filter warehouses by the client of the user (required)
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 

{
    "content": [
        {
            "warehouse": "LUMINOUS",
            "supplierProductSerial": "A00001A",
            "quantity": 15,
            "registrationDate": "2024-01-19T03:53:33.210+00:00",
            "updateDate": "2024-01-19T03:53:33.210+00:00"
        },
        {
            "warehouse": "LUMINOUS",
            "supplierProductSerial": "A00001B",
            "quantity": 4,
            "registrationDate": "2024-01-19T03:53:33.385+00:00",
            "updateDate": "2024-01-19T03:53:33.385+00:00"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageSize": 2,
        "pageNumber": 0,
        "paged": true,
        "unpaged": false
    },
    "last": false,
    "totalElements": 6,
    "totalPages": 3,
    "first": true,
    "size": 2,
    "number": 0,
    "sort": [],
    "numberOfElements": 2,
    "empty": false
}

- example :

http://localhost:8080/masterdata/warehouse-stock?warehouse=luminous&pageNumber=0&pageSize=2&user=gjimenez