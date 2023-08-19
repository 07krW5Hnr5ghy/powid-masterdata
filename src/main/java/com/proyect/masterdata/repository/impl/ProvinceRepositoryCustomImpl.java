package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.repository.ProvinceRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.List;

@Repository
public class ProvinceRepositoryCustomImpl implements ProvinceRepositoryCustom {

    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Province> searchForDepartment(String name, String user, Long idDepartment, String nameDepartment, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Province> criteriaQuery = criteriaBuilder.createQuery(Province.class);
        Root<Province> itemRoot = criteriaQuery.from(Province.class);
        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user, idDepartment, nameDepartment, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> provinceList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                provinceList =  listASC(sortColumn, criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                provinceList =  listDESC(sortColumn, criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(provinceList);
        } else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<Province> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name, user, idDepartment, nameDepartment, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable,count);
    }

    private List<Predicate> predicateConditions(String name, String user, Long idDepartment, String nameDepartment, Boolean status, CriteriaBuilder criteriaBuilder, Root<Province> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();
        if (name!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("user")), user.toUpperCase())));
        }

        if (idDepartment!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("department").get("id"), idDepartment)));
        }

        if (nameDepartment!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("department").get("name")), nameDepartment.toUpperCase())));
        }

        if (status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }
        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Province> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equals("NAME")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if (sortColumn.equals("USER")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        if (sortColumn.equals("IDDEPARTMENT")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("department").get("id")));
        }
        if (sortColumn.equals("DEPARTMENT")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("department").get("name")));
        }
        return departmentsList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Province> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equals("NAME")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if (sortColumn.equals("USER")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        if (sortColumn.equals("IDDEPARTMENT")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("department").get("id")));
        }
        if (sortColumn.equals("DEPARTMENT")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("department").get("name")));
        }
        return departmentsList;
    }

    private long getOrderCount(String name, String user, Long idDepartment, String nameDepartment, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Province> itemRoot = criteriaQuery.from(Province.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user, idDepartment, nameDepartment,status,criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
