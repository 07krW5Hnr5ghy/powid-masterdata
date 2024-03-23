package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.StockReplenishmentItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("stock-replenishment-item")
@AllArgsConstructor
public class StockReplenishmentItemController {
    private final IStockReplenishmentItem iStockReplenishmentItem;
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_ITEM_GET')")
    public ResponseEntity<Page<StockReplenishmentItemDTO>> list(
            @RequestParam("user") String user,
            @RequestParam(value = "orderId", required = false) Long orderId,
            @RequestParam(value = "productSku", required = false) String productSku,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<StockReplenishmentItemDTO> result = iStockReplenishmentItem.list(user,orderId,productSku,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:STOCK_REPLENISHMENT_ITEM_GET')")
    public ResponseEntity<List<StockReplenishmentItemDTO>> listStockReplenishmentItems(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<StockReplenishmentItemDTO> result = iStockReplenishmentItem.listStockReplenishmentItem(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
