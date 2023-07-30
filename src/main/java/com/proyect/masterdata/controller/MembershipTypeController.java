package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.request.RequestMasterList;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.impl.MembershipTypeImpl;
import io.swagger.v3.oas.annotations.Operation;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/membership-type")
@AllArgsConstructor
public class MembershipTypeController {
    private final MembershipTypeImpl iMembershipType;

    @Operation(summary = "Lista los tipos de membresias",
            description = "Lista los tipos de membresias")
    @GetMapping()
    public ResponseEntity<List<MasterListDTO>> listMembershipTypes() throws BadRequestExceptions {
        List<MasterListDTO> result = iMembershipType.listRecords();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @Operation(summary = "Registrar tipo de membresia",
            description = "Registrar tipo de membresia")
    @PostMapping()
    public ResponseEntity<ResponseMasterList> addMembershipType(@RequestParam("name") String name) throws BadRequestExceptions{
        ResponseMasterList result = iMembershipType.addRecord(name);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Borrar tipo de membresia",
            description = "Borrar tipo de membresia")
    @DeleteMapping()
    public ResponseEntity<ResponseMasterList> deleteMembershipType(@RequestParam("id") Long id) throws BadRequestExceptions{
        ResponseMasterList result = iMembershipType.deleteRecord(id);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @Operation(summary = "Editar tipo de membresia",
            description = "Editar tipo de membresia")
    @PutMapping()
    public ResponseEntity<MasterListDTO> updateMembership(@RequestBody RequestMasterList data) throws BadRequestExceptions{
        MasterListDTO result = iMembershipType.updateRecord(data.getName(), data.getId());
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
