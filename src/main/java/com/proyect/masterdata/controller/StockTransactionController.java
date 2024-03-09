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

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transaction")
@AllArgsConstructor
public class StockTransactionController {
    private final IStockTransaction iStockTransaction;
    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_TRANSACTION_GET')")
    public ResponseEntity<Page<StockTransactionDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "stockTransactionType", required = false) String stockTransactionType,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions {
        Page<StockTransactionDTO> result = iStockTransaction.list(user,serial,warehouse,stockTransactionType,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
