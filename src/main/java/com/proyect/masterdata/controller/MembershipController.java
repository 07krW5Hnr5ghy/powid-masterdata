package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.impl.MembershipImpl;
import io.swagger.v3.oas.annotations.Operation;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/membership")
@AllArgsConstructor
public class MembershipController {
    private final MembershipImpl iMembership;

    @Operation(summary = "Lista las membersias",
            description = "Lista las membresias")
    @GetMapping()
    public ResponseEntity<List<MasterListDTO>> listMemberships() throws BadRequestExceptions {
        List<MasterListDTO> result = iMembership.listRecords();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registrar membresia",
            description = "Registrar membresia")
    @PostMapping()
    public ResponseEntity<ResponseMasterList> addMembership(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponseMasterList result = iMembership.addRecord(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Borrar membresia",
            description = "Borrar membresia")
    @DeleteMapping()
    public ResponseEntity<ResponseMasterList> deleteMembership(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponseMasterList result = iMembership.deleteRecord(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar membresia",
            description = "Editar membresia")
    @PutMapping()
    public ResponseEntity<MasterListDTO> updateMembership(@RequestBody MasterListDTO data) throws BadRequestExceptions{
        MasterListDTO result = iMembership.updateRecord(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
