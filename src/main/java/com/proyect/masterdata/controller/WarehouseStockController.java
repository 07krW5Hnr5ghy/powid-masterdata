package com.proyect.masterdata.controller;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IWarehouseStock;

import lombok.AllArgsConstructor;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("warehouse-stock")
@AllArgsConstructor
public class WarehouseStockController {

    private final IWarehouseStock iWarehouseStock;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:WAREHOUSE_STOCK_GET')")
    public ResponseEntity<Page<WarehouseStockDTO>> list(
            @RequestParam(value = "warehouses", required = false) List<String> warehouses,
            @RequestParam(value = "serial",required = false) String serial,
            @RequestParam(value = "productSku",required = false) String productSku,
            @RequestParam(value = "model",required = false) String model,
            @RequestParam(value = "user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<WarehouseStockDTO>> result = iWarehouseStock.list(
                warehouses,
                serial,
                productSku,
                model,
                user,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:WAREHOUSE_STOCK_GET')")
    public ResponseEntity<List<WarehouseStockDTO>> listWarehouseStock(
            @RequestParam("user") String user,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "supplierProduct",required = false) String supplierProduct
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<WarehouseStockDTO>> result = iWarehouseStock.listWarehouse(user,warehouse,supplierProduct);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("supplier")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:WAREHOUSE_STOCK_GET')")
    public ResponseEntity<List<WarehouseStockDTO>> listWarehouseStockSupplier(
            @RequestParam("user") String user,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "supplier",required = false) String supplier
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<WarehouseStockDTO>> result = iWarehouseStock.listWarehouseAndSupplier(user,warehouse,supplier);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
