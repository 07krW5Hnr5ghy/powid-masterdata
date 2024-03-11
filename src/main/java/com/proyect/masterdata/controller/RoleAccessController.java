package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.RoleAccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IRoleAccess;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("role-access")
@AllArgsConstructor
public class RoleAccessController {
    private final IRoleAccess iRoleAccess;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ROLE_ACCESS_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("roleName") String roleName,
            @RequestParam("accessName") String accessName,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions{
        ResponseSuccess result = iRoleAccess.save(roleName,accessName,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ROLE_ACCESS_GET')")
    public ResponseEntity<Page<RoleAccessDTO>> list(
            @RequestParam(value = "roleName", required = false) String roleName,
            @RequestParam(value = "accessName", required = false) String accessName,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<RoleAccessDTO> result = iRoleAccess.list(roleName,accessName,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @GetMapping("status-false")
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ROLE_ACCESS_GET')")
    public ResponseEntity<Page<RoleAccessDTO>> listFalse(
            @RequestParam(value = "roleName", required = false) String roleName,
            @RequestParam(value = "accessName", required = false) String accessName,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber") Integer pageNumber,
            @RequestParam(value = "pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<RoleAccessDTO> result = iRoleAccess.listFalse(roleName,accessName,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ROLE_ACCESS_DELETE')")
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("roleName") String roleName,
            @RequestParam("accessName") String accessName,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions{
        ResponseDelete result = iRoleAccess.delete(roleName,accessName,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:ADMINISTRATION') and hasAuthority('ACCESS:ROLE_ACCESS_PUT')")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("roleName") String roleName,
            @RequestParam("accessName") String accessName,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions {
        ResponseSuccess result = iRoleAccess.activate(roleName,accessName,tokenUser);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }
}
