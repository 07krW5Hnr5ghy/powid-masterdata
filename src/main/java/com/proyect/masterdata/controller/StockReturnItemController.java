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
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<StockReturnItemDTO> result = iStockReturnItem.list(purchaseSerial, user,supplierProductSerial, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_ITEM_GET')")
    public ResponseEntity<List<StockReturnItemDTO>> listStockReturnItem(
            @RequestParam("user") String user
    ) throws InternalErrorExceptions,BadRequestExceptions {
        List<StockReturnItemDTO> result = iStockReturnItem.listStockReturnItem(user.toUpperCase());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_ITEM_GET')")
    public ResponseEntity<List<StockReturnItemDTO>> listStockReturnItemFalse(
            @RequestParam("user") String user
    ) throws InternalErrorExceptions,BadRequestExceptions {
        List<StockReturnItemDTO> result = iStockReturnItem.listStockReturnItemFalse(user.toUpperCase());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
