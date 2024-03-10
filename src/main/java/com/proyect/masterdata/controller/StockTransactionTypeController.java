package com.proyect.masterdata.controller;

import java.util.List;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.StockTransactionTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransactionType;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transaction-type")
@AllArgsConstructor
public class StockTransactionTypeController {
    private final IStockTransactionType iStockTransactionType;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:TRANSACTION_TYPE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iStockTransactionType.save(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "stock-transaction-types", consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:TRANSACTION_TYPE_POST')")
    public ResponseEntity<ResponseSuccess> saveAll(
            @RequestBody() List<String> names,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iStockTransactionType.saveAll(names, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:TRANSACTION_TYPE_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseDelete result = iStockTransactionType.delete(name, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:BUSINESS','ROLE:STOCK') and hasAuthority('ACCESS:TRANSACTION_TYPE_GET')")
    public List<StockTransactionTypeDTO> list() throws BadRequestExceptions {
        List<StockTransactionTypeDTO> result = iStockTransactionType.list();
        return result;
    }
}
