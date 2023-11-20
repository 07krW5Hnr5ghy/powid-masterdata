package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.request.RequestDistrictSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.DistrictMapper;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.DistrictRepositoryCustom;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IDistrict;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import java.sql.Date;
import java.util.Collections;
import java.util.List;

@Service
@AllArgsConstructor
@Log4j2
public class DistrictImpl implements IDistrict {

    private final DistrictRepository districtRepository;
    private final DistrictRepositoryCustom districtRepositoryCustom;
    private final ProvinceRepository provinceRepository;
    private final DistrictMapper districtMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name, String user, Long codeProvince)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsDistrict;
        boolean existsProvince;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsDistrict = districtRepository.existsByName(name.toUpperCase());
            existsProvince = provinceRepository.existsById(codeProvince);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (existsDistrict) {
            throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
        }

        if (!existsProvince) {
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        try {
            districtRepository.save(District.builder()
                    .name(name.toUpperCase())
                    .user(user.toUpperCase())
                    .idProvince(codeProvince)
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user, Long codeProvince)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsProvince;
        List<District> districts;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsProvince = provinceRepository.existsById(codeProvince);
            districts = districtRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!existsProvince) {
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }
        if (!districts.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorProvinceList.toUpperCase());
        }

        List<RequestDistrictSave> provinceSaves = names.stream().map(data -> RequestDistrictSave.builder()
                .user(user.toUpperCase())
                .codeProvince(codeProvince)
                .name(data.toUpperCase())
                .build()).toList();

        try {
            districtRepository.saveAll(districtMapper.listDistrictToListName(provinceSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public DistrictDTO update(RequestDistrict requestDistrict) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsprovince;
        District district;
        try {
            existsUser = userRepository.existsByUsername(requestDistrict.getUser().toUpperCase());
            existsprovince = provinceRepository.existsById(requestDistrict.getCodeProvince());
            district = districtRepository.findByNameAndStatusTrue(requestDistrict.getName().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (district == null) {
            throw new BadRequestExceptions(Constants.ErrorProvinceNotExist.toUpperCase());
        }

        if (!existsprovince) {
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        try {
            district.setName(requestDistrict.getName().toUpperCase());
            district.setUser(requestDistrict.getUser().toUpperCase());
            district.setDateRegistration(new Date(System.currentTimeMillis()));
            district.setStatus(requestDistrict.isStatus());
            district.setIdProvince(requestDistrict.getCodeProvince());
            return districtMapper.districtToDistrictDTO(districtRepository.save(district));
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        District district;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            district = districtRepository.findById(code).orElse(null);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (district == null) {
            throw new BadRequestExceptions(Constants.ErrorProvinceNotExist.toUpperCase());
        }

        try {
            district.setDateRegistration(new Date(System.currentTimeMillis()));
            district.setStatus(false);
            districtMapper.districtToDistrictDTO(districtRepository.save(district));
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public List<DistrictDTO> listDistrict() throws BadRequestExceptions {
        try {
            return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrue());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<DistrictDTO> list(String name, String user, Long codeProvince, String nameProvince, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<District> districtPage;
        try {
            districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                    sortColumn, pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (districtPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                districtPage.getPageable(), districtPage.getTotalElements());
    }

    @Override
    public Page<DistrictDTO> listStatusFalse(String name, String user, Long codeProvince, String nameProvince,
            String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<District> districtPage;
        try {
            districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                    sortColumn, pageNumber, pageSize, false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (districtPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                districtPage.getPageable(), districtPage.getTotalElements());
    }

    @Override
    public DistrictDTO findByCode(Long code) throws BadRequestExceptions {
        boolean exists;
        try {
            exists = districtRepository.existsById(code);
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (!exists) {
            throw new BadRequestExceptions(Constants.ErrorProvinceNotExist);
        }

        try {
            return districtMapper.districtToDistrictDTO(districtRepository.findById(code).orElse(null));
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
