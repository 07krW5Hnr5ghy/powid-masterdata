package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransactionItem;
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
@RequestMapping("stock-transaction-item")
@AllArgsConstructor
public class StockTransactionItemController {

    private final IStockTransactionItem iStockTransactionItem;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_ITEM_GET')")
    public ResponseEntity<Page<StockTransactionItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "stockTransaction", required = false) String stockTransaction,
            @RequestParam(value = "supplierProduct", required = false) String supplierProduct,
            @RequestParam(value = "warehouses",required = false) List<String> warehouses,
            @RequestParam(value = "stockTransactionTypes",required = false) List<String> stockTransactionTypes,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StockTransactionItemDTO>> result = iStockTransactionItem.list(
                user,
                stockTransaction,
                supplierProduct,
                warehouses,
                stockTransactionTypes,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_ITEM_GET')")
    public ResponseEntity<List<StockTransactionItemDTO>> listStockTransactionItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockTransactionItemDTO>> result = iStockTransactionItem.listStockTransactionItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
