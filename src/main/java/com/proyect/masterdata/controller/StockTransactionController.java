package com.proyect.masterdata.controller;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransactionItem;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transaction")
@AllArgsConstructor
public class StockTransactionController {

    private final IStockTransactionItem iStockTransactionItem;

    @GetMapping()
    public ResponseEntity<Page<StockTransactionItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<StockTransactionItemDTO> result = iStockTransactionItem.list(user, warehouse, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
