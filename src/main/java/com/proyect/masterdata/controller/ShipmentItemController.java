package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipmentItem;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment-item")
@AllArgsConstructor
public class ShipmentItemController {
    private final IShipmentItem iShipmentItem;
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SHIPMENT_ITEM_GET')")
    public ResponseEntity<Page<ShipmentItemDTO>> list(
            @RequestParam(value = "purchaseSerial", required = false) String purchaseSerial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "supplierProductSerial", required = false) String supplierProductSerial,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<ShipmentItemDTO> result = iShipmentItem.list(purchaseSerial, user, supplierProductSerial, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SHIPMENT_ITEM_GET')")
    public ResponseEntity<List<ShipmentItemDTO>> listShipment(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<ShipmentItemDTO> result = iShipmentItem.listShipmentItem(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
