package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.SupplyOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrderItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplyOrderItem;
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
@RequestMapping("supply-order-item")
@AllArgsConstructor
public class SupplyOrderItemController {
    private final ISupplyOrderItem iSupplyOrderItem;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("purchaseId") UUID purchaseId,
            @RequestBody() RequestSupplyOrderItem requestSupplyOrderItem,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, InterruptedException, ExecutionException {
        CompletableFuture<ResponseSuccess> result = iSupplyOrderItem.saveAsync(purchaseId, requestSupplyOrderItem,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_ITEM_GET')")
    public ResponseEntity<Page<SupplyOrderItemDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "supplier", required = false) String supplier,
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
        CompletableFuture<Page<SupplyOrderItemDTO>> result = iSupplyOrderItem.list(
                user,
                orderNumber,
                ref,
                warehouse,
                supplier,
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
    public ResponseEntity<List<SupplyOrderItemDTO>> listSupplyOrderItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id", required = false) UUID id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<SupplyOrderItemDTO>> result = iSupplyOrderItem.listSupplyOrderItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
