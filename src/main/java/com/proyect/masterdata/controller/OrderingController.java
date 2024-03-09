package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IOrdering;
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
@RequestMapping("order")
@AllArgsConstructor
public class OrderingController {

    private final IOrdering iOrdering;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAuthority('ROLE:SALES') and hasAuthority('ACCESS:ORDER_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestOrderSave requestOrderSave,
            @RequestParam("tokenUser") String tokenUser
    ) throws InternalErrorExceptions, BadRequestExceptions{
        ResponseSuccess result = iOrdering.save(requestOrderSave,tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE','ROLE:STOCK') and hasAuthority('ACCESS:ORDER_GET')")
    public ResponseEntity<Page<OrderDTO>> list(
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "orderState",required = false) String orderState,
            @RequestParam(value = "courier",required = false) String courier,
            @RequestParam(value = "paymentState",required = false) String paymentState,
            @RequestParam(value = "paymentMethod",required = false) String paymentMethod,
            @RequestParam(value = "saleChannel",required = false) String saleChannel,
            @RequestParam(value = "managementType",required = false) String managementType,
            @RequestParam(value = "sort",required = false) String sort,
            @RequestParam(value = "sortColumn",required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions{
        Page<OrderDTO> result = iOrdering.list(orderId,user,orderState,courier,paymentState,paymentMethod,saleChannel,managementType,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    @PreAuthorize("hasAnyAuthority('ROLE:SALES','ROLE:CUSTOMER_SERVICE') and hasAuthority('ACCESS:ORDER_PUT')")
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") Long orderId,
            @RequestBody()RequestOrderUpdate requestOrderUpdate,
            @RequestParam("tokenUser") String tokenUser
            ) throws BadRequestExceptions{
        ResponseSuccess result = iOrdering.update(orderId,requestOrderUpdate,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
