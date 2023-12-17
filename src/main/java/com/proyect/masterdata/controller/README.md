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
  "users":"11-50",
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

### GET /brand protected

- Description : list all active brands
- Request: none
- Parameters : 

1. name : filter by name of the brand (not required)
2. tokenUser : filter by user who register or updated the brand
3. sort : set the listing order only accepts the values ASC and DESC (not required)
4. sortColumn : set the column which sorts the list (not required)
5. pageNumber : select the page number to view of the list
6. pageSize : set how many records per page have the list

- Response : 

{
    "content": [
        {
            "name": "REEBOK",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "NIKE",
            "tokenUser": "ADMIN1"
        },
        {
            "name": "ADIDAS",
            "tokenUser": "ADMIN1"
        }
    ],
    "pageable": {
        "sort": [],
        "pageNumber": 0,
        "pageSize": 3,
        "offset": 0,
        "unpaged": false,
        "paged": true
    },
    "totalPages": 1,
    "totalElements": 3,
    "last": true,
    "first": true,
    "size": 3,
    "number": 0,
    "sort": [],
    "numberOfElements": 3,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/brand?pageNumber=0&pageSize=3&sort=DESC&sortColumn=name&tokenUser=admin1

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
3. user : username of the user who creates category

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

### GET /model

- Description : list all the active models registered in the database

- Request : none

- Parameters : 

1. name : filter models by name
2. user : filter models by the username who registered the modules
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response :

{
    "content": [
        {
            "name": "F50",
            "brand": "NIKE",
            "user": "ADMIN1"
        },
        {
            "name": "U PRO 3000",
            "brand": "NIKE",
            "user": "ADMIN1"
        },
        {
            "name": "K 20000",
            "brand": "NIKE",
            "user": "ADMIN1"
        }
    ],
    "pageable": {
        "sort": [],
        "offset": 0,
        "pageNumber": 0,
        "pageSize": 3,
        "paged": true,
        "unpaged": false
    },
    "last": true,
    "totalPages": 1,
    "totalElements": 3,
    "size": 3,
    "number": 0,
    "sort": [],
    "first": true,
    "numberOfElements": 3,
    "empty": false
}

- Example :

http://localhost:8080/masterdata/model?pageNumber=0&pageSize=3

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
        "mobile": "123456789",
        "dni": "12345678911",
        "category": "TENNIS",
        "usersMinimum": 11,
        "usersMaximum": 50,
        "billing": false,
        "comment": "administrar pedidos",
        "businessName": "TRADING COMPANY LTDA",
        "businessRuc": "12345678911",
        "gender": "M",
        "district": "ASUNCION",
        "store": "E-SHOP",
        "storeUrl": "e-shop.com",
        "storeType": "SHOPIFY",
        "closingChannels": [
            "INSTAGRAM",
            "FACEBOOK"
        ],
        "entryChannel": "TIKTOK",
        "demo": true
    }
    ... more onboarding records
]

- Example :

http://localhost:8080/masterdata/onboard

### GET /product protected

- Description : list all active products

- Request : none

- Parameters : 

1. sku : filter products by sku
2. user : filter products by the username who registered them
3. sort : sort the values the only valid values are ASC and DESC, default is ASC
4. sortColumn : select the value that sorts the list in this case name or user
5. pageNumber : the page number to select of the list the first is page zero (0) (required)
6. pageSize : number the records per Page (required)

- Response : 



- Example :

http://localhost:8080/masterdata/product?pageNumber=0&pageSize=2


### POST /product

- Description : register one product in the database

- Request : 

{
    sku : sku serial,
    model : name of model which the product belongs,
    color : name of the color of the product,
    category : name of the category of the product,
    size : name of the size of the product
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
    "size":"M"
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
        size : name of the size of the product
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

### POST /size-types protected

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
2. tokenUser : username of the user who creates size 

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

### POST /subscription protected

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

### POST /unit protected

- Description : add one unit to the database

- Request : none

- Parameters : 

1. name : name of the unit
2. tokenUser : username of the user who creates unit

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit?name=unidad&tokenUser=admin1

### POST /unit protected

- Description : add one or more units to the database

- Request : array with the name of the new units

- Parameters : 

1. tokenUser : username of the user who creates the units

- Response : 

{

    "code": 200,
    "message": "registration correctly"
}

- Example :

http://localhost:8080/masterdata/unit/units?tokenUser=admin1

### GET /unit protected

- Description : list all active units

- Request : none

- Parameters : none

- Response : 

[
    {
        "name": name of the unit
    }
    ... more units
]

- Example :

http://localhost:8080/masterdata/unit