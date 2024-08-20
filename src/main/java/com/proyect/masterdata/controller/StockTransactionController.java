package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockTransactionDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransaction;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transaction")
@AllArgsConstructor
public class StockTransactionController {
    private final IStockTransaction iStockTransaction;
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_GET')")
    public ResponseEntity<Page<StockTransactionDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "warehouses", required = false) List<String> warehouses,
            @RequestParam(value = "stockTransactionTypes", required = false) List<String> stockTransactionTypes,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StockTransactionDTO>> result = iStockTransaction.list(
                user,
                serials,
                warehouses,
                stockTransactionTypes,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_GET')")
    public ResponseEntity<List<StockTransactionDTO>> listStockTransaction(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockTransactionDTO>> result = iStockTransaction.listStockTransaction(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<StockTransactionDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockTransactionDTO>> result = iStockTransaction.listStockTransaction(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
