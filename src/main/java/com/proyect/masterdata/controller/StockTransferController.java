package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockTransferDTO;
import com.proyect.masterdata.dto.request.RequestStockTransfer;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransfer;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transfer")
@AllArgsConstructor
public class StockTransferController {
    private final IStockTransfer iStockTransfer;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestStockTransfer requestStockTransfer,
            @RequestParam() String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockTransfer.save(requestStockTransfer,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<StockTransferDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "originWarehouse",required = false) String originWarehouse,
            @RequestParam(value = "destinationWarehouse", required = false) String destinationWarehouse,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<StockTransferDTO> result = iStockTransfer.list(user,originWarehouse,destinationWarehouse,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
