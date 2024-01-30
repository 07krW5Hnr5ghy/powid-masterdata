package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.request.RequestOrderStock;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderStock;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock")
@AllArgsConstructor
public class OrderStockController {

    private final IOrderStock iOrderStock;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestBody() List<RequestOrderStock> requestOrderStockList,
            @RequestParam("tokenUser") String tokenUser
            ) throws InternalErrorExceptions, BadRequestExceptions{
        ResponseSuccess result = iOrderStock.save(orderId,requestOrderStockList,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<OrderStockDTO>> list(
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions {
        Page<OrderStockDTO> result = iOrderStock.list(warehouse,orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping(value = "list-false")
    public ResponseEntity<Page<OrderStockDTO>> listFalse(
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions {
        Page<OrderStockDTO> result = iOrderStock.listFalse(warehouse,orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
