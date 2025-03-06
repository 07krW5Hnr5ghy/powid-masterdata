package com.proyect.masterdata.controller;

import com.proyect.masterdata.domain.WarehouseOutputItem;
import com.proyect.masterdata.dto.WarehouseOutputDTO;
import com.proyect.masterdata.dto.WarehouseOutputItemDTO;
import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IWarehouseOutputItem;
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
@RequestMapping("warehouse-output-item")
@AllArgsConstructor
public class WarehouseOutputItemController {
    private final IWarehouseOutputItem iWarehouseOutputItem;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseSuccess> add(
            @RequestBody()RequestWarehouseOutputItem requestWarehouseOutputItem,
            @RequestParam("warehouseOutputId")UUID warehouseOutputId,
            @RequestParam("username") String username
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iWarehouseOutputItem.add(requestWarehouseOutputItem,warehouseOutputId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    public ResponseEntity<Page<WarehouseOutputItemDTO>> list(
            @RequestParam(value = "user") String user,
            @RequestParam(value = "orderNumber", required = false) Long orderNumber,
            @RequestParam(value = "ref", required = false) String ref,
            @RequestParam(value = "courier", required = false) String courier,
            @RequestParam(value = "warehouse", required = false) String warehouse,
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
        CompletableFuture<Page<WarehouseOutputItemDTO>> result = iWarehouseOutputItem.list(
                user,
                orderNumber,
                ref,
                courier,
                warehouse,
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
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("warehouseOutputId")UUID productId,
            @RequestParam("warehouseOutputId")UUID warehouseOutputId,
            @RequestParam("username") String username
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iWarehouseOutputItem.delete(productId,warehouseOutputId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping("update")
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("warehouseOutputId")UUID productId,
            @RequestParam("warehouseOutputId")UUID warehouseOutputId,
            @RequestParam("warehouseOutputId")Integer quantity,
            @RequestParam("username") String username
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iWarehouseOutputItem.updateQuantity(quantity,productId,warehouseOutputId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
