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

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transaction-item")
@AllArgsConstructor
public class StockTransactionItemController {

    private final IStockTransactionItem iStockTransactionItem;

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_ITEM_GET')")
    public ResponseEntity<Page<StockTransactionItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "stockTransactionSerial", required = false) String stockTransactionSerial,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<StockTransactionItemDTO> result = iStockTransactionItem.list(user, stockTransactionSerial, supplierProductSerial, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
