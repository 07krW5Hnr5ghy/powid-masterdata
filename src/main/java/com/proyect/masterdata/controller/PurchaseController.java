package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchase;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase")
@AllArgsConstructor
public class PurchaseController {
    private final IPurchase iPurchase;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestPurchase requestPurchase,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchase.saveAsync(requestPurchase, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<PurchaseDTO>> list(
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouses", required = false) List<String> warehouses,
            @RequestParam(value = "purchaseTypes", required = false) List<String> purchaseTypes,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseDTO>> result = iPurchase.list(
                serials,
                user,
                warehouses,
                purchaseTypes,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listPurchase(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseDTO>> result = iPurchase.listPurchase(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<PurchaseDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseDTO>> result = iPurchase.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("check-stock")
    public ResponseEntity<List<CheckStockDTO>> checkStock(
            @RequestParam("user") String user,
            @RequestParam("supplierProductId") UUID supplierProductId
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CheckStockDTO>> result = iPurchase.checkStock(supplierProductId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
