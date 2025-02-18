package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestSubCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISubCategoryProduct;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("sub-category-product")
@AllArgsConstructor
public class SubCategoryProductController {
    private final ISubCategoryProduct iSubCategoryProduct;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_POST')")
    public ResponseEntity<ResponseSuccess> save(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSubCategoryProduct.saveAsync(requestSubCategoryProduct);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("sku") String sku,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iSubCategoryProduct.delete(name,sku, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:CATEGORY_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("sku") String sku,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSubCategoryProduct.activate(name,sku,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
