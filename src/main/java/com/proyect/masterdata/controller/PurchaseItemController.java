package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-item")
@AllArgsConstructor
public class PurchaseItemController {

    private final IPurchaseItem iPurchaseItem;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<Page<PurchaseItemDTO>> list(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseItemDTO>> result = iPurchaseItem.list(serial, user,supplierProductSerial, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<List<PurchaseItemDTO>> listPurchaseItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseItemDTO>> result = iPurchaseItem.listPurchaseItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<List<PurchaseItemDTO>> listPurchaseItemFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseItemDTO>> result = iPurchaseItem.listPurchaseItemFalse(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
