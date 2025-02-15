package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.repository.DepartmentRepositoryCustom;
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
public class DepartamentRepositoryCustomImpl implements DepartmentRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;

    @Override
    public Page<Department> searchForDepartment(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Department> criteriaQuery = criteriaBuilder.createQuery(Department.class);
        Root<Department> itemRoot = criteriaQuery.from(Department.class);

        criteriaQuery.select(itemRoot);
        List<Predicate> conditions = predicateConditions(name, user, status,criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)){
            List<Order> departmentList = new ArrayList<>();
            if(sort.equalsIgnoreCase("ASC")){
                departmentList =  listASC(sortColumn, criteriaBuilder,itemRoot);
            }
            if(sort.equalsIgnoreCase("DESC")){
                departmentList =  listDESC(sortColumn, criteriaBuilder,itemRoot);
            }
            criteriaQuery.where(conditions.toArray(new Predicate[]{})).orderBy(departmentList);
        } else{
            criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        }

        TypedQuery<Department> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber*pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name,user, status);
        return new PageImpl<>(orderTypedQuery.getResultList(), pageable,count);
    }

    public List<Predicate> predicateConditions(String name, String user, Boolean status,
                                               CriteriaBuilder criteriaBuilder, Root<Department> itemRoot) {
        List<Predicate> conditions = new ArrayList<>();

        if (name!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if (user!=null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("user")), user.toUpperCase())));
        }

        if (status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status){
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Department> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equals("NAME")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }
        if (sortColumn.equals("USER")){
            departmentsList.add(criteriaBuilder.asc(itemRoot.get("user")));
        }
        return departmentsList;
    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<Department> itemRoot) {
        List<Order> departmentsList = new ArrayList<>();
        if (sortColumn.equalsIgnoreCase("NAME")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }
        if (sortColumn.equalsIgnoreCase("USER")){
            departmentsList.add(criteriaBuilder.desc(itemRoot.get("user")));
        }
        return departmentsList;
    }

    private long getOrderCount(String name, String user, Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Department> itemRoot = criteriaQuery.from(Department.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name, user, status,criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[]{}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
