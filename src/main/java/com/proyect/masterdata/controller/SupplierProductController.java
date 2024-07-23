package com.proyect.masterdata.controller;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

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
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSupplierProduct.saveAsync(requestSupplierProduct, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("serial") String serial,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iSupplierProduct.delete(serial, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("serial") String serial,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSupplierProduct.activate(serial, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<Page<SupplierProductDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "products", required = false) List<String> products,
            @RequestParam(value = "suppliers", required = false) List<String> suppliers,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {

        CompletableFuture<Page<SupplierProductDTO>> result = iSupplierProduct.list(
                user,
                serials,
                products,
                suppliers,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination-status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<Page<SupplierProductDTO>> listFalse(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "products", required = false) List<String> products,
            @RequestParam(value = "suppliers", required = false) List<String> suppliers,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<SupplierProductDTO>> result = iSupplierProduct.listFalse(
                user,
                serials,
                products,
                suppliers,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<List<SupplierProductDTO>> listSupplierProduct(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplierProductDTO>> result = iSupplierProduct.listSupplierProduct(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<List<SupplierProductDTO>> listSupplierProductFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplierProductDTO>> result = iSupplierProduct.listSupplierProductFalse(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("product")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SUPPLIER_PRODUCT_GET')")
    public ResponseEntity<List<SupplierProductDTO>> listSupplierProductProduct(
            @RequestParam("user") String user,
            @RequestParam(value = "productSku") String productSku
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplierProductDTO>> result = iSupplierProduct.listSupplierProductByProduct(user,productSku);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<SupplierProductDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplierProductDTO>> result = iSupplierProduct.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
