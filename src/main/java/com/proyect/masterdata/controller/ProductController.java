package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestProductUpdate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProduct;

import lombok.AllArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("product")
@AllArgsConstructor
public class ProductController {

    private final IProduct iProduct;

    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:PRODUCT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestPart("requestProduct") RequestProductSave product,
            @RequestPart("productPictures") MultipartFile[] productPictures,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iProduct.saveAsync(product,productPictures, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:PRODUCT_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("sku") String sku,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iProduct.delete(sku, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @PostMapping("activate")
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:PRODUCT_DELETE')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("sku") String sku,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iProduct.activate(sku, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:PRODUCT_GET')")
    public ResponseEntity<Page<ProductDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sku", required = false) String sku,
            @RequestParam(value = "model", required = false) String model,
            @RequestParam(value = "brands", required = false) List<String> brands,
            @RequestParam(value = "sizes", required = false) List<String> sizes,
            @RequestParam(value = "categoryProducts", required = false) List<String> categoryProducts,
            @RequestParam(value = "colors", required = false) List<String> colors,
            @RequestParam(value = "units", required = false) List<String> units,
            @RequestParam(value = "pictureFlag",required = false) Boolean pictureFlag,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ProductDTO>> result = iProduct.list(
                user,
                sku,
                model,
                brands,
                sizes,
                categoryProducts,
                colors,
                units,
                pictureFlag,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping(value = "pagination/list-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:PRODUCT_GET')")
    public ResponseEntity<Page<ProductDTO>> listFalse(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sku", required = false) String sku,
            @RequestParam(value = "model", required = false) String model,
            @RequestParam(value = "brands", required = false) List<String> brands,
            @RequestParam(value = "sizes", required = false) List<String> sizes,
            @RequestParam(value = "categoryProducts", required = false) List<String> categoryProducts,
            @RequestParam(value = "colors", required = false) List<String> colors,
            @RequestParam(value = "units", required = false) List<String> units,
            @RequestParam(value = "pictureFlag",required = false) Boolean pictureFlag,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ProductDTO>> result = iProduct.listFalse(
                user,
                sku,
                model,
                brands,
                sizes,
                categoryProducts,
                colors,
                units,
                pictureFlag,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:PRODUCT_GET')")
    public ResponseEntity<List<ProductDTO>> listProducts(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listProducts(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:PRODUCT_GET')")
    public ResponseEntity<List<ProductDTO>> listProductsFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listProductsFalse(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("model")
    //@PreAuthorize("hasAnyAuthority('ROLE:MARKETING','ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:PRODUCT_GET')")
    public ResponseEntity<List<ProductDTO>> listProductsModel(
            @RequestParam("user") String user,
            @RequestParam("model") String model
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listProductsModel(user,model);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<ProductDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions,ExecutionException,InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ResponseSuccess> addPictures(
            @RequestPart("productPictures") List<MultipartFile> productPictures,
            @RequestPart("requestProductUpdate")RequestProductUpdate requestProductUpdate
            ) throws BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<ResponseSuccess> result = iProduct.update(requestProductUpdate,productPictures);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("color-size")
    public ResponseEntity<List<ProductDTO>> listByColorAndSize(
            @RequestParam("user") String user,
            @RequestParam("color") String color,
            @RequestParam("size") String size
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listByColorAndSize(color,size,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("model-color-size")
    public ResponseEntity<List<ProductDTO>> listByModelAndColorAndSize(
            @RequestParam("user") String user,
            @RequestParam("model") String model,
            @RequestParam("color") String color,
            @RequestParam("size") String size
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listByModelAndSizeAndColor(model,size,color,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("model-color")
    public ResponseEntity<List<ProductDTO>> listByModelAndColor(
            @RequestParam("user") String user,
            @RequestParam("model") String model,
            @RequestParam("color") String color
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ProductDTO>> result = iProduct.listByModelAndColor(model,color,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
