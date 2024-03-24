package com.proyect.masterdata.controller;

import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.CategoryProductDTO;
import com.proyect.masterdata.dto.request.RequestCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICategoryProduct;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("category-product")
@AllArgsConstructor
public class CategoryProductController {

    private final ICategoryProduct iCategoryProduct;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_PRODUCT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("description") String description,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iCategoryProduct.save(name, description, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "category-products", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_PRODUCT_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestCategoryProduct> categoryProducts,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iCategoryProduct.saveAll(categoryProducts, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:CATEGORY_PRODUCT_GET')")
    public ResponseEntity<Page<CategoryProductDTO>> list(
            @RequestParam(value = "name", required = false) String name,
            @RequestParam(value = "user", required = false) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<CategoryProductDTO> result = iCategoryProduct.list(name, user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION','ROLE:MARKETING','ROLE:STOCK') and hasAuthority('ACCESS:CATEGORY_PRODUCT_GET')")
    public ResponseEntity<List<CategoryProductDTO>> listCategoryProducts() throws BadRequestExceptions {
        List<CategoryProductDTO> result = iCategoryProduct.listCategoryProducts();
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
