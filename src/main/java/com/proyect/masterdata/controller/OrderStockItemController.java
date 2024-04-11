package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.OrderStockItemDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderStockItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("order-stock-item")
@AllArgsConstructor
public class OrderStockItemController {

    private final IOrderStockItem iOrderStockItem;

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
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

    @GetMapping(value = "pagination/status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
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

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<List<OrderStockItemDTO>> listOrderStockItem(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions {
        List<OrderStockItemDTO> result = iOrderStockItem.listOrderStockItem(user,id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:ORDER_STOCK_ITEM_GET')")
    public ResponseEntity<List<OrderStockItemDTO>> listOrderStockItemFalse(
            @RequestParam("user") String user,
            @RequestParam(value = "id",required = false) Long id
    ) throws BadRequestExceptions {
        List<OrderStockItemDTO> result = iOrderStockItem.listOrderStockItem(user,id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProduct,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseDelete result = iOrderStockItem.delete(orderId,supplierProduct,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<ResponseSuccess> update(
            @RequestParam("orderId") Long orderId,
            @RequestParam("supplierProduct") String supplierProduct,
            @RequestParam("tokenUser") String tokenUser,
            @RequestParam("quantity") Integer quantity
    )throws BadRequestExceptions {
        ResponseSuccess result = iOrderStockItem.update(orderId,supplierProduct,tokenUser,quantity);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
