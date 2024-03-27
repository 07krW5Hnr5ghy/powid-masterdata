package com.proyect.masterdata.controller;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.SupplierProductDTO;
import com.proyect.masterdata.dto.request.RequestSupplierProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplierProduct;

import lombok.AllArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("supplier-product")
@AllArgsConstructor
public class SupplierProductController {

    private final ISupplierProduct iSupplierProduct;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestSupplierProduct requestSupplierProduct,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSupplierProduct.save(requestSupplierProduct, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "supplier-products", consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<RequestSupplierProduct> requestSupplierProductsList,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iSupplierProduct.saveAll(requestSupplierProductsList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("serial") String serial,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iSupplierProduct.delete(serial, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<Page<SupplierProductDTO>> list(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "productSku", required = false) String productSku,
            @RequestParam(value = "supplierRuc", required = false) String supplierRuc,
            @RequestParam(value = "purchasePrice", required = false) Double purchasePrice,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {

        Page<SupplierProductDTO> result = iSupplierProduct.list(serial, user,productSku,supplierRuc,purchasePrice, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "pagination-status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<Page<SupplierProductDTO>> listFalse(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "productSku", required = false) String productSku,
            @RequestParam(value = "supplierRuc", required = false) String supplierRuc,
            @RequestParam(value = "purchasePrice", required = false) Double purchasePrice,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<SupplierProductDTO> result = iSupplierProduct.listFalse(serial, user,productSku,supplierRuc,purchasePrice, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<List<SupplierProductDTO>> listSupplierProduct(
            @RequestParam("user") String user,
            @RequestParam(value = "supplierRuc",required = false) String supplierRuc
    ) throws BadRequestExceptions {
        List<SupplierProductDTO> result = iSupplierProduct.listSupplierProduct(user,supplierRuc);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<List<SupplierProductDTO>> listSupplierProductFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "supplierRuc",required = false) String supplierRuc
    ) throws BadRequestExceptions {
        List<SupplierProductDTO> result = iSupplierProduct.listSupplierProductFalse(user,supplierRuc);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
