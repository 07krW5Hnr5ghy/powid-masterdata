package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockTransferItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockTransferItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-transfer-item")
@AllArgsConstructor
public class StockTransferItemController {

    private final IStockTransferItem iStockTransferItem;

    @GetMapping()
    public ResponseEntity<Page<StockTransferItemDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "stockTransferId",required = false) Long stockTransferId,
            @RequestParam(value = "supplierProductSerial",required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<StockTransferItemDTO> result = iStockTransferItem.list(user,stockTransferId,supplierProductSerial,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
