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

### POST /category
- Description : add one category to the database
- Request: none
- Parameters : 

1. name : name of category
2. description : description of the category
3. user : username of the user who creates category

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/category?name=tennis&description=zapatos%20para%20correr&user=admin1

### POST /category/categories
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

http://localhost:8080/masterdata/category/categories?user=admin1

Body

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

### POST /closing-channel

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

### POST /department

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

### POST /district

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

### POST /entry-channel

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

### GET /module

- Description : list the modules registered in the database

- Request : none

- Parameters : 

1. name : filter modules by name
2. user : filter modules by the username who registered the modules
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0)
6. pageSize : number the records per Page

- Response : 

{
    "content": [
        {
            "moduleName": "VENTAS",
            "modulePrice": 2.57
        },
        {
            "moduleName": "FINANZAS",
            "modulePrice": 2.57
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
    "totalElements": 5,
    "totalPages": 1,
    "first": true,
    "size": 5,
    "number": 0,
    "sort": [],
    "numberOfElements": 5,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/module?pageNumber=0&pageSize=5&sort=ASC&sortColumn=name


### POST /module

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

### POST /province

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

### POST /store-type

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

- Description : list the subscriptions registered in the database

- Request : none

- Parameters : 

1. name : filter subscriptions by name
2. user : filter subscriptions by the username who registered them
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0)
6. pageSize : number the records per Page

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

### POST /subscription

- Description : add one subscription to the database

- Request : none

- Parameters : 

1. name : name of the subscription
2. months : months of duration for the subscription
3. price : price of the subscription for one month
2. tokenUser : username of the user who creates the subscription 

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/subscription?name=semestral&months=6&discountPercent=5.00&tokenUser=admin1