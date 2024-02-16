package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestStockTransfer;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransfer;
import lombok.AllArgsConstructor;
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
            @RequestBody() List<RequestStockTransferItem> requestStockTransferItemList,
            @RequestParam() String tokenUser
            ) throws BadRequestExceptions {
        ResponseSuccess result = iStockTransfer.save(requestStockTransfer,requestStockTransferItemList,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
