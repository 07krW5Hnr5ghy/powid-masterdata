package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IStockReturnItem;
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
@RequestMapping("stock-return-item")
@AllArgsConstructor
public class StockReturnItemController {
    private final IStockReturnItem iStockReturnItem;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_ITEM_GET')")
    public ResponseEntity<Page<StockReturnItemDTO>> list(
            @RequestParam(value = "purchaseSerial", required = false) String purchaseSerial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<StockReturnItemDTO>> result = iStockReturnItem.list(purchaseSerial, user,supplierProductSerial, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_ITEM_GET')")
    public ResponseEntity<List<StockReturnItemDTO>> listStockReturnItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id", required = false) Long id
    ) throws InternalErrorExceptions, BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockReturnItemDTO>> result = iStockReturnItem.listStockReturnItem(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_ITEM_GET')")
    public ResponseEntity<List<StockReturnItemDTO>> listStockReturnItemFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws InternalErrorExceptions, BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<StockReturnItemDTO>> result = iStockReturnItem.listStockReturnItemFalse(user,id);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
