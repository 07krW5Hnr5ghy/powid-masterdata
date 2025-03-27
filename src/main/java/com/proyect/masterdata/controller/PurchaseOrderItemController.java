package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PurchaseOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseOrderItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase-order-item")
@AllArgsConstructor
public class PurchaseOrderItemController {
    private final IPurchaseOrderItem iPurchaseOrderItem;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("purchaseId") UUID purchaseId,
            @RequestBody() RequestPurchaseOrderItem requestPurchaseOrderItem,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<ResponseSuccess> result = iPurchaseOrderItem.saveAsync(purchaseId, requestPurchaseOrderItem,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<Page<PurchaseOrderItemDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "quantity", required = false) Integer quantity,
            @RequestParam(value = "product", required = false) String product,
            @RequestParam(value = "model", required = false) String model,
            @RequestParam(value = "color", required = false) String color,
            @RequestParam(value = "size", required = false) String size,
            @RequestParam(value = "status", required = false) Boolean status,
            @RequestParam(value = "registrationStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationStartDate,
            @RequestParam(value = "registrationEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime registrationEndDate,
            @RequestParam(value = "updateStartDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateStartDate,
            @RequestParam(value = "updateEndDate",required = false) @DateTimeFormat(iso=DateTimeFormat.ISO.DATE) OffsetDateTime updateEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<PurchaseOrderItemDTO>> result = iPurchaseOrderItem.list(
                user,
                orderNumber,
                ref,
                quantity,
                product,
                model,
                color,
                size,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize,
                status);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<List<PurchaseOrderItemDTO>> listPurchaseOrderItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id", required = false) UUID id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<PurchaseOrderItemDTO>> result = iPurchaseOrderItem.listPurchaseOrderItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:COLOR_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("purchaseId") UUID purchaseId,
            @RequestParam("productId") UUID productId,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iPurchaseOrderItem.delete(purchaseId,productId,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("purchaseId") UUID purchaseId,
            @RequestParam("productId") UUID productId,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iPurchaseOrderItem.activate(purchaseId,productId,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
