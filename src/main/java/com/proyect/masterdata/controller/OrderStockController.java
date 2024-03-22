package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrderStock;
import com.proyect.masterdata.services.IOrderStockItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock")
@AllArgsConstructor
public class OrderStockController {

    private final IOrderStock iOrderStock;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:ORDER_STOCK_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("orderId") Long orderId,
            @RequestParam("warehouse") String warehouse,
            @RequestBody() List<RequestOrderStockItem> requestOrderStockItemList,
            @RequestParam("tokenUser") String tokenUser
            ) throws InternalErrorExceptions, BadRequestExceptions{
        ResponseSuccess result = iOrderStock.save(orderId,warehouse, requestOrderStockItemList,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STOCK_GET')")
    public ResponseEntity<Page<OrderStockDTO>> list(
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "orderId",required = false) Long orderId,
            @RequestParam("user") String user,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = false) Integer pageNumber,
            @RequestParam(value = "pageSize", required = false) Integer pageSize
    ) throws BadRequestExceptions{
        Page<OrderStockDTO> result = iOrderStock.list(warehouse,orderId,user,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ORDER_STOCK_GET')")
    public ResponseEntity<List<OrderStockDTO>> listOrderStock(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<OrderStockDTO> result = iOrderStock.listOrderStock(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
