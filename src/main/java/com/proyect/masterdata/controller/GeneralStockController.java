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

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.WarehouseStockDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IGeneralStock;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("general-stock")
@AllArgsConstructor
public class GeneralStockController {

    private final IGeneralStock iGeneralStock;

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:ADMINISTRATION','ROLE:STOCK') and hasAuthority('ACCESS:GENERAL_STOCK_GET')")
    public ResponseEntity<Page<GeneralStockDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<GeneralStockDTO> result = iGeneralStock.list(user, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
