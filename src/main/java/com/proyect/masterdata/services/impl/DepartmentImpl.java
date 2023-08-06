package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.DepartmentPK;
import com.proyect.masterdata.dto.DepartmentDTO;
import com.proyect.masterdata.dto.request.RequestDepartment;
import com.proyect.masterdata.dto.request.RequestDepartmentSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.DepartmentMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.services.IDepartment;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import java.util.List;


@Service
@AllArgsConstructor
@Log4j2
public class DepartmentImpl implements IDepartment {

    private final DepartmentRepository departmentRepository;
    private final DepartmentMapper departmentMapper;

    @Override
    public ResponseSuccess save(String name,String user) throws BadRequestExceptions {
        try {
            departmentRepository.save(departmentMapper.departmentToName(RequestDepartmentSave
                    .builder().name(name.toUpperCase()).user(user.toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user) throws BadRequestExceptions{
        try {
            List<RequestDepartmentSave> departmentSaves = names.stream().map(data -> RequestDepartmentSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            departmentRepository.saveAll(departmentMapper.listDepartmentToListName(departmentSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public DepartmentDTO update(RequestDepartment requestDepartment) throws BadRequestExceptions {
        try {
            requestDepartment.setName(requestDepartment.getName().toUpperCase());
            requestDepartment.setUser(requestDepartment.getUser().toUpperCase());
            Department department = departmentRepository.save(departmentMapper.requestDepartmentToDepartment(requestDepartment));
            return departmentMapper.departmentToDepartmentDTO(department);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions{
        try {
            departmentRepository.deleteById(DepartmentPK.builder()
                            .id(code)
                            .user(user)
                    .build());
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes, String user) throws BadRequestExceptions{
        try {
            List<DepartmentPK> departmentPKList = codes.stream().map(data -> DepartmentPK.builder()
                    .user(user.toUpperCase())
                    .id(data)
                    .build()).toList();
            departmentRepository.deleteAllById(departmentPKList);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<DepartmentDTO> list() throws BadRequestExceptions{
        try {
            return departmentMapper.listDepartmentToListDepartmentDTO(departmentRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DepartmentDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return departmentMapper.listDepartmentToListDepartmentDTO(departmentRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DepartmentDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public DepartmentDTO findByName(String name) throws BadRequestExceptions{
        try {
            return departmentMapper.departmentToDepartmentDTO(departmentRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<DepartmentDTO> findByUser(String user) throws BadRequestExceptions{
        try {
            return departmentMapper.listDepartmentToListDepartmentDTO(departmentRepository.findByLoginUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
