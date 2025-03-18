package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.SupplyOrderDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplyOrder;
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
@RequestMapping("supply-order")
@AllArgsConstructor
public class SupplyOrderController {
    private final ISupplyOrder iSupplyOrder;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestSupplyOrder requestSupplyOrder,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSupplyOrder.saveAsync(requestSupplyOrder, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<SupplyOrderDTO>> list(
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "supplier", required = false) String supplier,
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
        CompletableFuture<Page<SupplyOrderDTO>> result = iSupplyOrder.list(
                orderNumber,
                ref,
                user,
                warehouse,
                supplier,
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
    @GetMapping("check-stock")
    public ResponseEntity<List<CheckStockDTO>> checkStock(
            @RequestParam("user") String user,
            @RequestParam("supplierProductId") UUID supplierProductId
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CheckStockDTO>> result = iSupplyOrder.checkStock(supplierProductId,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseDelete> close(
            @RequestParam("username") String username,
            @RequestParam("warehouseOutputId") UUID supplyOrderId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iSupplyOrder.closeSupplyOrder(supplyOrderId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
