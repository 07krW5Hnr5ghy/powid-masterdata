package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PurchaseOrderDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseOrder;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-order")
@AllArgsConstructor
public class PurchaseOrderController {
    private final IPurchaseOrder iPurchaseOrder;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestPurchaseOrder requestPurchaseOrder,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchaseOrder.saveAsync(requestPurchaseOrder, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<PurchaseOrderDTO>> list(
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize,
            @RequestParam(value = "status", required = false) Boolean status
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseOrderDTO>> result = iPurchaseOrder.list(
                orderNumber,
                ref,
                user,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseDelete> close(
            @RequestParam("username") String username,
            @RequestParam("purchaseOrderId") UUID purchaseOrderId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iPurchaseOrder.closePurchaseOrder(purchaseOrderId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
