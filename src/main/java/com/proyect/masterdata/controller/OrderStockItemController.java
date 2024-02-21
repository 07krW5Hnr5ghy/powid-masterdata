package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderStockItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock-item")
@AllArgsConstructor
public class OrderStockItemController {

    private final IOrderStockItem iOrderStockItem;

    @GetMapping()
    public ResponseEntity<Page<OrderStockItemDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions {
        Page<OrderStockItemDTO> result = iOrderStockItem.list(user,orderId,supplierProductSerial,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "list-false")
    public ResponseEntity<Page<OrderStockItemDTO>> listFalse(
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions {
        Page<OrderStockItemDTO> result = iOrderStockItem.listFalse(warehouse,orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
