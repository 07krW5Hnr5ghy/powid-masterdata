package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProductPrice;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("product-price")
@AllArgsConstructor
public class ProductPriceController {

    private final IProductPrice iProductPrice;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:MARKETING') and hasAuthority('ACCESS:PRODUCT_PRICE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("productSku") String productSku,
            @RequestParam("unitPrice") Double unitPrice,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iProductPrice.saveAsync(productSku,unitPrice,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

}
