package com.proyect.masterdata.controller;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.services.IPurchase;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseItem;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase")
@AllArgsConstructor
public class PurchaseController {

    private final IPurchase iPurchase;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestPurchase requestPurchase
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchase.saveAsync(requestPurchase);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<PurchaseDTO>> list(
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "documents", required = false) List<String> documents,
            @RequestParam(value = "suppliers",required = false) List<String> suppliers,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseDTO>> result = iPurchase.list(
                serials,
                user,
                documents,
                suppliers,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listPurchase(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseDTO>> result = iPurchase.listPurchase(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listPurchaseFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseDTO>> result = iPurchase.listPurchaseFalse(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listFilters(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseDTO>> result = iPurchase.listPurchaseFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

}
