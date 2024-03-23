package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReturn;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-return")
@AllArgsConstructor
public class StockReturnController {
    private final IStockReturn iStockReturn;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:STOCK_RETURN_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() List<RequestStockReturnItem> requestStockReturnItemList,
            @RequestParam("purchaseSerial") String purchaseSerial,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockReturn.save(purchaseSerial,requestStockReturnItemList,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_GET')")
    public ResponseEntity<Page<StockReturnDTO>> list(
            @RequestParam(value = "purchaseSerial", required = false) String purchaseSerial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<StockReturnDTO> result = iStockReturn.list(purchaseSerial, user, sort, sortColumn, pageNumber,
                pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_GET')")
    public ResponseEntity<List<StockReturnDTO>> listStockReturn(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<StockReturnDTO> result = iStockReturn.listStockReturn(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_RETURN_GET')")
    public ResponseEntity<List<StockReturnDTO>> listStockReturnFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<StockReturnDTO> result = iStockReturn.listStockReturnFalse(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
